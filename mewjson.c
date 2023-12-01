// mewjson: a library for (de)serializing JSON text
// Distributed under the MIT License (http://opensource.org/licenses/MIT)
#include "mewjson.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZEOF(x) (JsonSize)sizeof(x)
#define ALIGNOF(x) (JsonSize) _Alignof(x)
#define COUNTOF(a) (SIZEOF(a) / SIZEOF(*(a)))
#define LENGTHOF(a) (COUNTOF(a) - 1)
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

// Object representing an unowned, contiguous sequence of bytes
struct Slice {
    const char *ptr;
    JsonSize len;
};

#define SLICE_NEW(p, l)                                                                            \
    (struct Slice)                                                                                 \
    {                                                                                              \
        .ptr = (p), .len = (l)                                                                     \
    }

#define SLICE_STR(s)                                                                               \
    (struct Slice)                                                                                 \
    {                                                                                              \
        .ptr = (s), .len = strlen(s)                                                               \
    }

void jsonParserInit(struct JsonParser *parser, struct JsonAllocator *a)
{
    static const struct JsonAllocator kPrototype = {
        .malloc = malloc,
        .realloc = realloc,
        .free = free,
    };
    parser->x.a = a ? *a : kPrototype;
}

#define JSON_MALLOC(a, size) (a).malloc((size_t)(size))
#define JSON_REALLOC(a, ptr, size) (a).realloc(ptr, (size_t)(size))
#define JSON_FREE(a, ptr) (a).free(ptr)

typedef uint64_t ValueTag;
// 3 bits are needed to represent the 6 possible JSON value types. The rest of the bits are used
// to store the size of the value. For containers, the size is the number of child nodes, and for
// strings, it is the number of bytes in the string. The other types have fixed sizes, so we
// just set the size field to 0.
#define VTAG_TYPE(vt) (enum JsonType)((vt)&7)
#define VTAG_SIZE(vt) (JsonSize)((vt) >> 3)
#define MAKE_VTAG(t, s) (ValueTag)((t) | (s) << 3)

static void vTagSetSize(ValueTag *tag, JsonSize n)
{
    *tag = MAKE_VTAG(VTAG_TYPE(*tag), n);
}

// Node in a JSON parse tree
struct JsonValue {
    // Type and size information for the node
    ValueTag tag;

    // Data contained in the node, or the offset of the parent relative to the current
    // node ("up" variant used for containers)
    union {
        JsonSize up;
        const char *string;
        int64_t integer;
        double real;
        JsonBool boolean;
    } v;
};

#define ISCONTAINER(t) ((t) >= kTypeObject)

static JsonBool isObject(const JsonValue *v)
{
    return jsonType(v) == kTypeObject;
}

// Represents a JSON parse tree
struct JsonDocument {
    struct JsonAllocator a;

    // Array of JSON value nodes
    JsonValue *values;
    JsonSize length;
    JsonSize capacity;

    // Padded JSON text that was parsed to create this document
    char text[];
};

static JsonValue *docFetchValue(JsonDocument *doc, JsonSize index)
{
    assert(index >= 0 && index < doc->length);
    return &doc->values[index];
}

static JsonValue *docAppendValue(struct JsonParser *parser, enum JsonType type)
{
    JsonDocument *doc = parser->x.doc;
    if (doc->length >= doc->capacity) {
        JsonSize cap = 4;
        while (cap <= doc->length) {
            cap *= 2;
        }
        // Resize the buffer to fit cap pointers to JsonValue.
        JsonValue *ptr = JSON_REALLOC(parser->x.a, doc->values, cap * SIZEOF(*doc->values));
        if (!ptr) {
            parser->status = kParseNoMemory;
            return NULL;
        }
        doc->values = ptr;
        doc->capacity = cap;
    }
    const JsonSize up = parser->x.upIndex;
    // Size is 1 for every value type except containers and strings. Strings store the number of
    // characters and containers store the total number of children (recursively). String length
    // is set by the caller.
    ValueTag tag = 1;
    if (ISCONTAINER(type)) {
        parser->x.upIndex = doc->length;
        tag = 0;
    }
    JsonValue *val = &doc->values[doc->length];
    *val = (JsonValue){
        .tag = MAKE_VTAG(type, tag),
        .v = {.up = up - doc->length},
    };
    ++doc->length;
    return val;
}

static void docAppendString(struct JsonParser *parser, struct Slice s)
{
    JsonValue *val = docAppendValue(parser, kTypeString);
    if (!val) {
        return;
    }
    vTagSetSize(&val->tag, s.len);
    val->v.string = s.ptr;
}

static void docAppendInteger(struct JsonParser *parser, int64_t i)
{
    JsonValue *val = docAppendValue(parser, kTypeInteger);
    if (val) {
        val->v.integer = i;
    }
}

static void docAppendReal(struct JsonParser *parser, double r)
{
    JsonValue *val = docAppendValue(parser, kTypeReal);
    if (val) {
        val->v.real = r;
    }
}

static void docAppendBoolean(struct JsonParser *parser, JsonBool b)
{
    JsonValue *val = docAppendValue(parser, kTypeBoolean);
    if (val) {
        val->v.boolean = b;
    }
}

static void docAppendNull(struct JsonParser *parser)
{
    docAppendValue(parser, kTypeNull);
}

enum {
    kShiftValue = 4,    // xxxx >>>>
    kMaskIsSpace = 1,   // 0000 0001
    kMaskIsFp = 2,      // 0000 0010
    kMaskIsNumeric = 4, // 0000 0100
    kMaskIsHex = 12,    // 0000 1100
};

// Character class lookup table: 4 least-significant bits of each byte store flags indicating what
// class the character belongs to: one of "space", "floating-point indicator", "numeric digit", or
// "hex digit". 'e' and 'E' belong to both the "floating-point indicator" and "hex digit" classes.
// The masks and shift values above are used to test for inclusion in one of the classes, and to
// extract the value bits for numeric and hex digits.
static const uint8_t kCharClassTable[256] = {
    0,   0,   0,   0,   0,   0,  0,  0,  0,  1,   1,   0,   0,   1,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   1,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   2,   0,   4,   20, 36, 52, 68, 84,  100, 116, 132, 148, 0,   0, 0, 0, 0, 0, 0, 168,
    184, 200, 216, 234, 248, 0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  168, 184, 200, 216, 234, 248, 0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,  0,  0,  0,  0,   0,   0,   0,   0,
};

#define ISSPACE(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsSpace)
#define ISFPCHAR(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsFp)
#define ISNUMERIC(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsNumeric)
#define ISHEX(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsHex)
#define NUMVAL(c) (kCharClassTable[(uint8_t)(c)] >> kShiftValue)
#define HEXVAL(c) (kCharClassTable[(uint8_t)(c)] >> kShiftValue)

enum Token {
    kTokenString,
    kTokenValue,
    kTokenBeginObject,
    kTokenEndObject,
    kTokenBeginArray,
    kTokenEndArray,
    kTokenNameSeparator,
    kTokenValueSeparator,
    kTokenEndOfInput,
    kTokenError,
    kTokenCount
};

static char peekChar(const char **ptr)
{
    return (*ptr)[0];
}

static char getChar(const char **ptr)
{
    ++*ptr;
    return (*ptr)[-1];
}

#define GET_BYTE(parser) ((uint8_t)getChar(ptr))

// Move the parser cursor back by 1 byte
// Note that this function does not alter the parser's "c" field (the current character).
static void ungetChar(const char **ptr)
{
    // getChar() is always called at least once before ungetChar(). If the document is completely
    // empty, the parse is not attempted.
    --*ptr;
}

static char skipWhitespace(const char **ptr)
{
    char c;
    do {
        c = getChar(ptr);
    } while (ISSPACE(c));
    return c;
}

MEWJSON_NODISCARD
static int getCodepoint(const char **ptr)
{
    char c[4];
    if (!ISHEX(c[0] = getChar(ptr)) || //
        !ISHEX(c[1] = getChar(ptr)) || //
        !ISHEX(c[2] = getChar(ptr)) || //
        !ISHEX(c[3] = getChar(ptr))) { //
        return -1;
    }
    return HEXVAL(c[0]) << 12 | //
           HEXVAL(c[1]) << 8 |  //
           HEXVAL(c[2]) << 4 |  //
           HEXVAL(c[3]);        //
}

static enum Token scanNull(struct JsonParser *parser, const char **ptr)
{
    if (getChar(ptr) == 'u' && //
        getChar(ptr) == 'l' && //
        getChar(ptr) == 'l') { //
        docAppendNull(parser);
        return kTokenValue;
    }
    parser->status = kParseLiteralInvalid;
    return kTokenError;
}

static enum Token scanTrue(struct JsonParser *parser, const char **ptr)
{
    if (getChar(ptr) == 'r' && //
        getChar(ptr) == 'u' && //
        getChar(ptr) == 'e') { //
        docAppendBoolean(parser, 1);
        return kTokenValue;
    }
    parser->status = kParseLiteralInvalid;
    return kTokenError;
}

static enum Token scanFalse(struct JsonParser *parser, const char **ptr)
{
    if (getChar(ptr) == 'a' && //
        getChar(ptr) == 'l' && //
        getChar(ptr) == 's' && //
        getChar(ptr) == 'e') { //
        docAppendBoolean(parser, 0);
        return kTokenValue;
    }
    parser->status = kParseLiteralInvalid;
    return kTokenError;
}

MEWJSON_NODISCARD
static JsonSize scanUtf8(const char **ptr, char **out, char byte)
{
    // Table and shifting trick from @skeeto/branchless-utf8. Only using half of the original
    // table, since 1-byte codepoints have already been handled.
    static const uint8_t kLengths[] = {0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0};
    uint8_t c[4] = {(uint8_t)byte};
    assert(c[0] >= 0x80); // ASCII chars already excluded

    char *end = *out;
    uint32_t codepoint;
    switch (kLengths[(c[0] >> 3) - 16]) {
        case 2:
            if (((c[1] = GET_BYTE(ptr)) & 0xC0) != 0x80) {
                return -1;
            }
            *end++ = (char)c[0];
            *end++ = (char)c[1];
            codepoint = ((uint32_t)(c[0] & 0x1F) << 6) | //
                        ((uint32_t)(c[1] & 0x3F) << 0);
            break;
        case 3:
            if (((c[1] = GET_BYTE(ptr)) & 0xC0) != 0x80 || //
                ((c[2] = GET_BYTE(ptr)) & 0xC0) != 0x80) {
                return -1;
            }
            *end++ = (char)c[0];
            *end++ = (char)c[1];
            *end++ = (char)c[2];
            codepoint = ((uint32_t)(c[0] & 0x0F) << 12) | //
                        ((uint32_t)(c[1] & 0x3F) << 6) |  //
                        ((uint32_t)(c[2] & 0x3F) << 0);
            break;
        case 4:
            if (((c[1] = GET_BYTE(ptr)) & 0xC0) != 0x80 || //
                ((c[2] = GET_BYTE(ptr)) & 0xC0) != 0x80 || //
                ((c[3] = GET_BYTE(ptr)) & 0xC0) != 0x80) {
                return -1;
            }
            *end++ = (char)c[0];
            *end++ = (char)c[1];
            *end++ = (char)c[2];
            *end++ = (char)c[3];
            codepoint = ((uint32_t)(c[0] & 0x07) << 18) | //
                        ((uint32_t)(c[1] & 0x3F) << 12) | //
                        ((uint32_t)(c[2] & 0x3F) << 6) |  //
                        ((uint32_t)(c[3] & 0x3F) << 0);
            break;
        default:
            return -1;
    }
    *out = end;
    return codepoint >= 0xD800 && codepoint <= 0xDFFF ? -1 : 0;
}

static enum Token scanString(struct JsonParser *parser, const char **ptr)
{
    // Cast away constness. *ptr points into heap-allocated memory. This routine unescapes
    // strings inplace, since we have already made a copy of the whole text. The unescaping
    // process only ever decreases the length of a string, so this will always work.
    char *out = (char *)*ptr;
    const char *begin = *ptr;
    for (;;) {
        // If c is not EOF, then we can call getChar() at least 4 times without going
        // past the end of the buffer.
        const char c = getChar(ptr);
        switch (c) {
            case '\\':
                switch (getChar(ptr)) {
                    case '"':
                        *out++ = '"';
                        break;
                    case '\\':
                        *out++ = '\\';
                        break;
                    case '/':
                        *out++ = '/';
                        break;
                    case 'b':
                        *out++ = '\b';
                        break;
                    case 'f':
                        *out++ = '\f';
                        break;
                    case 'n':
                        *out++ = '\n';
                        break;
                    case 'r':
                        *out++ = '\r';
                        break;
                    case 't':
                        *out++ = '\t';
                        break;
                    case 'u': {
                        int codepoint = getCodepoint(ptr);
                        if (codepoint < 0) {
                            parser->status = kParseStringInvalidCodepoint;
                            return kTokenError;
                        }
                        if (0xD800 <= codepoint && codepoint <= 0xDFFF) {
                            // Codepoint is part of a surrogate pair. Expect a high surrogate
                            // (U+D800–U+DBFF) followed by a low surrogate (U+DC00–U+DFFF).
                            if (codepoint <= 0xDBFF) {
                                if (getChar(ptr) != '\\' || getChar(ptr) != 'u') {
                                    parser->status = kParseStringInvalidCodepoint;
                                    return kTokenError;
                                }
                                const int codepoint2 = getCodepoint(ptr);
                                if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                                    parser->status = kParseStringMissingLowSurrogate;
                                    return kTokenError;
                                }
                                codepoint = (((codepoint - 0xD800) << 10) | (codepoint2 - 0xDC00)) +
                                            0x10000;
                            } else {
                                parser->status = kParseStringMissingHighSurrogate;
                                return kTokenError;
                            }
                        }
                        // Translate the codepoint into bytes. Modified from @Tencent/rapidjson.
                        if (codepoint <= 0x7F) {
                            *out++ = (char)codepoint;
                        } else if (codepoint <= 0x7FF) {
                            *out++ = (char)(0xC0 | ((codepoint >> 6) & 0xFF));
                            *out++ = (char)(0x80 | ((codepoint & 0x3F)));
                        } else if (codepoint <= 0xFFFF) {
                            *out++ = (char)(0xE0 | ((codepoint >> 12) & 0xFF));
                            *out++ = (char)(0x80 | ((codepoint >> 6) & 0x3F));
                            *out++ = (char)(0x80 | (codepoint & 0x3F));
                        } else {
                            assert(codepoint <= 0x10FFFF);
                            *out++ = (char)(0xF0 | ((codepoint >> 18) & 0xFF));
                            *out++ = (char)(0x80 | ((codepoint >> 12) & 0x3F));
                            *out++ = (char)(0x80 | ((codepoint >> 6) & 0x3F));
                            *out++ = (char)(0x80 | (codepoint & 0x3F));
                        }
                        break;
                    }
                    default:
                        parser->status = kParseStringInvalidEscape;
                        return kTokenError;
                }
                break;
            case '"': {
                // Closing double quote finishes the string.
                const struct Slice string = (struct Slice){
                    .ptr = begin,
                    .len = out - begin,
                };
                docAppendString(parser, string);
                return kTokenString;
            }
            default:
                if ((uint8_t)c < 0x20) {
                    parser->status = kParseStringUnescapedControl;
                    return kTokenError;
                } else if ((uint8_t)c < 0x80) {
                    *out++ = c;
                } else if (scanUtf8(ptr, &out, c)) {
                    parser->status = kParseStringInvalidUtf8;
                    return kTokenError;
                }
        }
    }
}

static enum Token scanReal(struct JsonParser *parser, const char **ptr, JsonBool isNegative)
{
    const char *begin = *ptr - isNegative;
    // According to RFC 8259, the ABNF for a number looks like:
    //     [ minus ] int [ frac ] [ exp ]
    // We have already parsed the "[ minus ]" and "int" parts. Either the "int" part overflowed, or
    // there exists a "frac" or "exp" part that needs to be scanned. Since parsing floating-point
    // numbers is extremely complicated, we are using strtod(). All we have to do is make sure the
    // rest of the number is formatted correctly, and leave the cursor on the first character that
    // is not part of the number.
#define SKIP_DIGITS                                                                                \
    while (ISNUMERIC(peekChar(ptr))) {                                                             \
        getChar(ptr);                                                                              \
    }

    // If an integer overflowed, then there might be some digits left. Skip any remaining
    // digits in the integral part.
    SKIP_DIGITS

    char c = peekChar(ptr);
    if (c == '.') {
        getChar(ptr);
        // Consume the fractional part.
        if (!ISNUMERIC(peekChar(ptr))) {
            parser->status = kParseNumberMissingFraction;
            return kTokenError;
        }
        SKIP_DIGITS
        c = peekChar(ptr);
    }
    if (c == 'e' || c == 'E') {
        getChar(ptr);
        // Consume the exponential part.
        c = peekChar(ptr);
        if (c == '+' || c == '-') {
            getChar(ptr);
        }
        if (!ISNUMERIC(peekChar(ptr))) {
            parser->status = kParseNumberMissingExponent;
            return kTokenError;
        }
        SKIP_DIGITS
    }
    // Input has been padded with at least 1 byte that strtod() will not recognize as being part
    // of a real number. Otherwise, strtod() will continue reading past the end of the buffer.
    docAppendReal(parser, strtod(begin, NULL));
    return kTokenValue;
}

static enum Token scanNumber(struct JsonParser *parser, const char **ptr, JsonBool isNegative)
{
    const char *begin = *ptr;
    if (!ISNUMERIC(peekChar(ptr))) {
        // Catches cases like "-e2", "-E5", and "-".
        goto return_with_error;
    }
    if (getChar(ptr) == '0') {
        if (ISNUMERIC(peekChar(ptr))) {
            // '0' followed by another digit.
            goto return_with_error;
        }
    }
    ungetChar(ptr);

    int64_t value = 0;
    while (ISNUMERIC(peekChar(ptr))) {
        const unsigned v = NUMVAL(getChar(ptr));
        if (value > INT64_MAX / 10) {
            // This number is definitely too large to be represented as an int64_t (the
            // "value * 10" below will cause signed integer overflow. Parse it as a double
            // instead.
            goto call_scan_real;
        } else if (value == INT64_MAX / 10) {
            // This number might be too large (the "+ v" below might cause an overflow).
            // Handle the special cases (modified from SQLite). Note that INT64_MAX is equal
            // to 9223372036854775807. In this branch, v is referring to the least-significant
            // digit of INT64_MAX, since the value * 10 below will yield 9223372036854775800.
            if (v == 9) {
                goto call_scan_real;
            } else if (v == 8) {
                if (isNegative && !ISNUMERIC(peekChar(ptr))) {
                    docAppendInteger(parser, INT64_MIN);
                    return kTokenValue;
                }
                goto call_scan_real;
            }
        }
        value = value * 10 + v;
    }

    if (!ISFPCHAR(peekChar(ptr))) {
        assert(value != INT64_MIN);
        docAppendInteger(parser, isNegative ? -value : value);
        return kTokenValue;
    }

call_scan_real:
    *ptr = begin;
    return scanReal(parser, ptr, isNegative);
return_with_error:
    parser->status = kParseNumberInvalid;
    return kTokenError;
}

static enum Token scanToken(struct JsonParser *parser, const char **ptr)
{
    // skipWhitespace(parser) returns the first non-whitespace character following the last
    // token we read.
    switch (skipWhitespace(ptr)) {
        case '"':
            return scanString(parser, ptr);
        case '-':
            return scanNumber(parser, ptr, 1);
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            ungetChar(ptr);
            return scanNumber(parser, ptr, 0);
        case 'n':
            return scanNull(parser, ptr);
        case 't':
            return scanTrue(parser, ptr);
        case 'f':
            return scanFalse(parser, ptr);
        case ':':
            return kTokenNameSeparator;
        case ',':
            return kTokenValueSeparator;
        case '{':
            docAppendValue(parser, kTypeObject);
            return kTokenBeginObject;
        case '[':
            docAppendValue(parser, kTypeArray);
            return kTokenBeginArray;
        case '}':
            return kTokenEndObject;
        case ']':
            return kTokenEndArray;
        case EOF:
            return kTokenEndOfInput;
        default:
            parser->status = kParseSyntaxError;
            return kTokenError;
    }
}

// States for the parser
enum State {
    kStateError,
    kStateEnd,
    kStateBegin,
    kAB, // Array begin
    kA1, // Array element
    kAx, // Array element separator
    kAE, // Array end
    kOB, // Object begin
    kO1, // Object key
    kOx, // Object key separator
    kO2, // Object value
    kOy, // Object value separator
    kOE, // Object end
    kV1, // Freestanding value
    kStateCount
};

// Predict the next state based on the current state and a token read by the parser
// This idea is from @Tencent/rapidjson.
static enum State predictState(enum State src, enum Token token)
{
    // Note the comments to the right of each table row. The kStateBegin state (named "beg"
    // below) is marked "source" because it is the starting state. If each state is imagined
    // to be a vertex in a directed graph, and each state transition an edge, then kStateBegin
    // has no edges leading into it. Likewise, states marked "sink" have no edges leading out
    // of them (or more accurately, all edges lead to the error state). If a state is marked
    // "push", then we are entering a nested object or array. We need to remember what type of
    // structure we are currently in, so we push the current state onto a stack. A "pop" state
    // indicates that control is leaving a nested structure. The top of the stack is popped off
    // to reveal the type of structure the parser has just moved back into. It should be noted
    // that the stack is not maintained as a separate data structure. It is implicit in the DOM
    // structure being built.
    // Note that all "pop" states are also sinks. This is because the parser needs information
    // contained in the stack in order to make a decision on what state to transition into.
    // The decision cannot be made in this function. It is the responsibility of transit() to
    // make sure the parser is transitioned to either kA1 or kO2, depending on the type of
    // structure that the parser has moved back into, so that additional values in that
    // structure can be parsed properly.
    // clang-format off
    static const uint8_t kTransitions[kStateCount][kTokenCount] = {
#define END kStateEnd
#define ERR kStateError
            // Token = str  val   {    }    [    ]    :    ,   end  err
            /* ERR */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR}, // sink
            /* END */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR}, // sink
            /* beg */ {kV1, kV1, kOB, ERR, kAB, ERR, ERR, ERR, ERR, ERR}, // source
            /* kAB */ {kA1, kA1, kOB, ERR, kAB, kAE, ERR, ERR, ERR, ERR}, // push
            /* kA1 */ {ERR, ERR, ERR, ERR, ERR, kAE, ERR, kAx, ERR, ERR},
            /* kAx */ {kA1, kA1, kOB, ERR, kAB, ERR, ERR, ERR, ERR, ERR},
            /* kAE */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR}, // pop
            /* kOB */ {kO1, ERR, ERR, kOE, ERR, ERR, ERR, ERR, ERR, ERR}, // push
            /* kO1 */ {ERR, ERR, ERR, ERR, ERR, ERR, kOx, ERR, ERR, ERR},
            /* kOx */ {kO2, kO2, kOB, ERR, kAB, ERR, ERR, ERR, ERR, ERR},
            /* kO2 */ {ERR, ERR, ERR, kOE, ERR, ERR, ERR, kOy, ERR, ERR},
            /* kOy */ {kO1, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR},
            /* kOE */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR}, // pop
            /* kV1 */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, END, ERR}, // sink
#undef END
#undef ERR
    };
    // clang-format on
    return kTransitions[src][token];
}

MEWJSON_NODISCARD
static enum State parserBeginContainer(struct JsonParser *parser, enum State nextState)
{
    ++parser->x.depth;
    if (parser->x.depth > MEWJSON_MAX_DEPTH) {
        parser->status = kParseExceededMaxDepth;
        return kStateError;
    }
    return nextState;
}

MEWJSON_NODISCARD
static enum State parserEndContainer(struct JsonParser *parser)
{
    // This precondition is ensured by the state transition table in predictState(). This
    // function will never be called unless parserBeginContainer() was called at some point.
    assert(parser->x.depth > 0);
    JsonDocument *doc = parser->x.doc;
    assert(parser->x.upIndex < doc->length);
    // up is the address of the parent JSON value.
    JsonValue *up = docFetchValue(doc, parser->x.upIndex);
    const JsonSize index = doc->length - 1;
    vTagSetSize(&up->tag, index - parser->x.upIndex);
    assert(ISCONTAINER(jsonType(up)));
    parser->x.upIndex += up->v.up;

    --parser->x.depth;
    if (parser->x.depth > 0) {
        up = docFetchValue(doc, parser->x.upIndex);
        // Pretend like the parser just finished reading an object member value or an array
        // element. Essentially, we are considering the whole object or array that we just
        // left to be a single value, rather than a compound structure. We have to check
        // the grandparent, because the kOE token has not been accepted yet (acceptToken()
        // below). The parser control is technically still inside the object or array
        // presently being closed.
        return isObject(up) ? kO2 : kA1;
    }
    // Just closed the root-level structure, so we must be finished. It is an error
    // otherwise, since RFC 8259 only allows a singular root.
    return kStateEnd;
}

// Transition into the next state
static enum State transitState(struct JsonParser *parser, enum State dst)
{
    // `dst` is the next state as predicted by predict(), upon reading token `token`.
    // We need to make sure that the destination state is not a transient state (either
    // kOE or kAE). As described in predict(), kOE (object end) and kAE (array end) are
    // "pop" states, meaning we pop an element off the stack and leave a nested structure.
    // After doing so, we examine the new stack top. Using that value, we transition to
    // either kO2 (object member value) or kA1 (array element). Normally, these states
    // are entered when we have just read an object member value or array element,
    // respectively, so basically, we are just treating the object or array as a child
    // member/element in its parent object/array.
    switch (dst) {
        case kAB:
        case kOB:
            return parserBeginContainer(parser, dst);
        case kAE:
        case kOE:
            return parserEndContainer(parser);
        default:
            return dst;
    }
}

static enum Token parserAdvance(struct JsonParser *parser, const char **ptr)
{
    if (parser->status == kParseOk) {
        return scanToken(parser, ptr);
    }
    return kTokenError;
}

static int parserFinish(struct JsonParser *parser, enum State state, const char **ptr)
{
    skipWhitespace(ptr);
    if (parser->status != kParseOk) {
        return -1;
    }
    if (parser->x.depth != 0) {
        // The state transition mechanism prevents the depth from ever getting below 0. Once
        // the last container has been closed, the parser transitions into the end state, a
        // sink state, and refuses to accept any more tokens.
        assert(parser->x.depth > 0);
        parser->status = kParseContainerNotClosed;
        return -1;
    }
    if (state != kStateEnd) {
        // Non-specific syntax error.
        parser->status = kParseSyntaxError;
        return -1;
    }
    return 0;
}

static int parserParse(struct JsonParser *parser, const char **ptr)
{
    enum State state = kStateBegin;
    enum Token token;
    do {
        token = parserAdvance(parser, ptr);
        state = predictState(state, token);
        state = transitState(parser, state);
    } while (state > kStateBegin);
    return parserFinish(parser, state, ptr);
}

static void parserFinalize(struct JsonParser *parser, const char *ptr, JsonSize length)
{
    const JsonSize offset = ptr - parser->x.doc->text;
    parser->offset = MIN(offset, length);
    if (parser->status != kParseOk) {
        goto handle_error;
    }
    // The char * used to iterate over the input buffer should be 1 or more bytes past
    // the first padding byte (skipWhitespace() is called in parserFinish() to skip past
    // trailing whitespace). Otherwise, there is junk at the end of the input.
    if (offset <= length) {
        parser->status = kParseSyntaxError;
        goto handle_error;
    }
    return;

handle_error:
    jsonDestroyDocument(parser->x.doc);
    parser->x.doc = NULL;
}

#define PADDING_LENGTH 4

MEWJSON_NODISCARD
JsonDocument *jsonRead(const char *input, JsonSize length, struct JsonParser *parser)
{
    struct JsonAllocator a = parser->x.a;
    *parser = (struct JsonParser){.offset = -1, .x = {.a = a}};
    JsonDocument *doc = JSON_MALLOC(a, sizeof(JsonDocument) + (size_t)length + PADDING_LENGTH);
    if (!doc) {
        parser->status = kParseNoMemory;
        return NULL;
    }
    memcpy(doc->text, input, (size_t)length);
    memset(doc->text + length, EOF, PADDING_LENGTH);
    *doc = (JsonDocument){.a = a};
    parser->x.doc = doc;

    // Parse the document.
    const char *ptr = doc->text;
    parserParse(parser, &ptr);
    parserFinalize(parser, ptr, length);
    return parser->x.doc;
}

struct Writer {
    char *ptr;
    JsonSize len;
    JsonSize acc;
};

static void appendChar(struct Writer *w, char c)
{
    ++w->acc;
    if (w->len > 0) {
        w->ptr[0] = c;
        ++w->ptr;
        --w->len;
    }
}

static void appendString(struct Writer *w, struct Slice s)
{
    w->acc += s.len;
    if (s.len > 0 && s.len <= w->len) {
        memcpy(w->ptr, s.ptr, (size_t)s.len);
        w->ptr += s.len;
        w->len -= s.len;
    }
}

static void appendFiniteReal(struct Writer *w, double v)
{
    assert(isfinite(v));

    char buf[32];
    static const int kMaxDigits = DBL_DECIMAL_DIG;
    JsonSize len = snprintf(buf, COUNTOF(buf), "%.*g", kMaxDigits, v);
    // Make sure the printed number contains some indication that it is a real number rather than
    // an integer. This step is very important: otherwise, we get strange problems, like "-0.0"
    // being written as "-0", which is stored as the integer 0. Tests that attempt to roundtrip
    // "-0.0", or any other real that rounds to it, will fail. Also converts ',' to '.'  (some
    // locales use ',' as their decimal point, but RFC 8259 specifies '.').
    JsonBool foundFpChar = 0;
    for (JsonSize i = 0; i < len; i++) {
        if (buf[i] == ',') {
            buf[i] = '.';
        }
        if (ISFPCHAR(buf[i])) {
            foundFpChar = 1;
        }
    }
    if (!foundFpChar) {
        buf[len++] = '.';
        buf[len++] = '0';
    }
    appendString(w, SLICE_NEW(buf, len));
}

// NOTE: This function is a bit faster than using snprintf(), and has the added benefit of not
//       requiring inclusion of inttypes.h to get the proper format specifier ("%" PRId64).
static void appendInteger(struct Writer *w, int64_t integer)
{
    // This buffer should have a single extra byte in the worst case.
    char buffer[32];
    const JsonBool negative = integer < 0;
    char *end = buffer + COUNTOF(buffer);
    char *ptr = end - 1;
    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement systems.
    uint64_t u = integer == INT64_MIN ? (1ULL << 63) : (uint64_t)llabs(integer);
    do {
        *ptr-- = (char)(u % 10 + '0');
        u /= 10;
    } while (u);
    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }
    appendString(w, SLICE_NEW(ptr, (JsonSize)(end - ptr)));
}

// Modified from SQLite (src/json.c).
static void appendEscapedString(struct Writer *w, struct Slice s)
{
    appendChar(w, '"');
    for (JsonSize i = 0; i < s.len; i++) {
        uint8_t c = ((uint8_t *)s.ptr)[i];
        if (c == '"' || c == '\\') {
json_simple_escape:
            appendChar(w, '\\');
        } else if (c <= 0x1F) {
            static const char kSpecialChars[] = {
                0, 0, 0, 0, 0, 0, 0, 0, 'b', 't', 'n', 0, 'f', 'r', 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,   0,   0,   0, 0,   0,   0, 0,
            };
            assert(COUNTOF(kSpecialChars) == 32);
            assert(kSpecialChars['\b'] == 'b');
            assert(kSpecialChars['\f'] == 'f');
            assert(kSpecialChars['\n'] == 'n');
            assert(kSpecialChars['\r'] == 'r');
            assert(kSpecialChars['\t'] == 't');
            if (kSpecialChars[c]) {
                c = (uint8_t)kSpecialChars[c];
                goto json_simple_escape;
            }
            appendChar(w, '\\');
            appendChar(w, 'u');
            appendChar(w, '0');
            appendChar(w, '0');
            appendChar(w, (char)('0' + (c >> 4)));
            c = (uint8_t) "0123456789abcdef"[c & 0xF];
        }
        appendChar(w, (char)c);
    }
    appendChar(w, '"');
}

static const char kContainerBegin[] = "[{";
static const char kContainerEnd[] = "]}";

static void writeScalar(struct Writer *w, struct JsonCursor *c)
{
    assert(!ISCONTAINER(jsonType(c->value)));
    switch (jsonType(c->value)) {
        case kTypeString:
            appendEscapedString(w, SLICE_NEW(c->value->v.string, VTAG_SIZE(c->value->tag)));
            break;
        case kTypeInteger:
            appendInteger(w, c->value->v.integer);
            break;
        case kTypeBoolean:
            appendString(w, c->value->v.boolean ? SLICE_STR("true") : SLICE_STR("false"));
            break;
        case kTypeReal:
            if (isfinite(c->value->v.real)) {
                appendFiniteReal(w, c->value->v.real);
                break;
            }
            // Intentional fallthrough
        default: // kTypeNull
            appendString(w, SLICE_STR("null"));
            break;
    }
}

static void writeEmptyContainer(struct Writer *w, struct JsonCursor *c)
{
    static const struct Slice kEmptyObject = {.ptr = "{}", .len = 2};
    static const struct Slice kEmptyArray = {.ptr = "[]", .len = 2};
    appendString(w, isObject(c->value) ? kEmptyObject : kEmptyArray);
}

static JsonBool cursorInObject(struct JsonCursor *c)
{
    return c->value != c->parent && isObject(c->parent);
}

static void cursorNext(struct JsonCursor *c)
{
    assert(jsonCursorIsValid(c));
    ++c->value;
}

static JsonSize numberOfChildren(const JsonValue *val)
{
    if (ISCONTAINER(jsonType(val))) {
        return VTAG_SIZE(val->tag);
    }
    return 0;
}

static void cursorAdjustParent(struct JsonCursor *c)
{
    c->parent += c->parent->v.up;
}

static JsonBool cursorEnterNonEmptyContainer(struct JsonCursor *c)
{
    if (ISCONTAINER(jsonType(c->value)) && numberOfChildren(c->value) > 0) {
        c->parent = c->value;
        return 1;
    }
    return 0;
}

void jsonCursorInit(JsonValue *root, enum JsonCursorMode mode, struct JsonCursor *c)
{
    *c = (struct JsonCursor){
        .root = root,
        .parent = root,
        .value = root,
        .mode = mode,
    };
    if (mode == kCursorNormal) {
        // Enter the container and maybe skip the key.
        ++c->value;
        if (isObject(root)) {
            c->value += jsonLength(root) > 0;
        }
    }
}

JsonBool jsonCursorIsValid(const struct JsonCursor *c)
{
    return c->value && c->value - c->root <= numberOfChildren(c->root);
}

void jsonCursorNext(struct JsonCursor *c)
{
    assert(jsonCursorIsValid(c));
    if (c->mode == kCursorNormal) {
        c->value += numberOfChildren(c->value);
    } else if (cursorEnterNonEmptyContainer(c)) {
        // Entered a nested container.
    } else {
        while (c->parent != c->root && c->value - c->parent >= VTAG_SIZE(c->parent->tag)) {
            // Finished a container. Correct the parent pointer.
            cursorAdjustParent(c);
        }
    }
    cursorNext(c);
    if (jsonCursorIsValid(c)) {
        // Skip the key so that c->value is positioned on the object member value.
        c->value += cursorInObject(c);
    }
}

const char *jsonCursorKey(struct JsonCursor *c, JsonSize *length)
{
    if (c->value == c->parent || !isObject(c->parent)) {
        return NULL;
    }
    JsonValue *key = c->value - 1;
    if (length) {
        *length = jsonLength(key);
    }
    return key->v.string;
}

MEWJSON_NODISCARD
JsonSize jsonWrite(char *buffer, JsonSize length, JsonValue *root)
{
    struct Writer w = {
        .ptr = buffer,
        .len = length,
    };
    struct JsonCursor c;
    jsonCursorInit(root, kCursorRecursive, &c);
    if (!ISCONTAINER(jsonType(root))) {
        // Root is not a container. Write out the single value and return.
        writeScalar(&w, &c);
        return w.acc;
    } else if (VTAG_SIZE(root->tag) == 0) {
        // Root container is empty.
        writeEmptyContainer(&w, &c);
        return w.acc;
    }
    for (;;) {
        if (cursorInObject(&c)) {
            // Write an object key. Advance to the value.
            assert(jsonType(c.value) == kTypeString);
            appendEscapedString(&w, SLICE_NEW(c.value->v.string, VTAG_SIZE(c.value->tag)));
            appendChar(&w, ':');
            cursorNext(&c);
        }
        if (ISCONTAINER(jsonType(c.value))) {
            // Value is a nested container.
            if (cursorEnterNonEmptyContainer(&c)) {
                appendChar(&w, kContainerBegin[isObject(c.value)]);
                cursorNext(&c);
                continue;
            }
            writeEmptyContainer(&w, &c);
        } else {
            // Value is a scalar.
            writeScalar(&w, &c);
        }
        cursorNext(&c);

next_iteration:
        if (c.value - c.parent <= VTAG_SIZE(c.parent->tag)) {
            appendChar(&w, ',');
        } else {
            // Back out of nested containers. This block doesn't move the cursor at all, it just
            // adjusts the parent pointer until a container is found with children that still need
            // to be written.
            appendChar(&w, kContainerEnd[isObject(c.parent)]);
            if (c.parent == c.root) {
                // Reached the end of the JSON value acting as root.
                break;
            }
            cursorAdjustParent(&c);
            goto next_iteration;
        }
    }
    return w.acc;
}

enum JsonType jsonType(const JsonValue *val)
{
    return VTAG_TYPE(val->tag);
}

const char *jsonString(const JsonValue *val)
{
    assert(jsonType(val) == kTypeString);
    return val->v.string;
}

int64_t jsonInteger(const JsonValue *val)
{
    assert(jsonType(val) == kTypeInteger);
    return val->v.integer;
}

double jsonReal(const JsonValue *val)
{
    assert(jsonType(val) == kTypeReal);
    return val->v.real;
}

JsonBool jsonBoolean(const JsonValue *val)
{
    assert(jsonType(val) == kTypeBoolean);
    return val->v.boolean;
}

JsonSize jsonLength(const JsonValue *val)
{
    if (jsonType(val) == kTypeString) {
        return VTAG_SIZE(val->tag);
    } else if (!ISCONTAINER(jsonType(val))) {
        return 0;
    }
    // Account for the fact that separate nodes are created for object member name and value.
    const JsonSize isObj = isObject(val);
    JsonSize num = numberOfChildren(val);
    JsonSize len = 0;
    ++val; // Move to first child

    while (num > 0) {
        const JsonSize step = 1 + numberOfChildren(val);
        val += step;
        num -= step;
        ++len;
    }
    assert(num == 0);
    return isObj ? len / 2 : len;
}

JsonValue *jsonRoot(JsonDocument *doc)
{
    assert(doc && doc->values);
    return &doc->values[0];
}

void jsonDestroyDocument(JsonDocument *doc)
{
    if (doc) {
        JSON_FREE(doc->a, doc->values);
        JSON_FREE(doc->a, doc);
    }
}

JsonSize jsonObjectFind(JsonValue *obj, const char *key, JsonSize length)
{
    assert(isObject(obj));
    struct JsonCursor c;
    jsonCursorInit(obj, kCursorNormal, &c);
    for (JsonSize i = 0; jsonCursorIsValid(&c); ++i) {
        JsonSize n;
        const char *cursorKey = jsonCursorKey(&c, &n);
        if (length == n && 0 == memcmp(cursorKey, key, (size_t)length)) {
            return i;
        }
        jsonCursorNext(&c);
    }
    return -1;
}

JsonValue *jsonContainerGet(JsonValue *container, JsonSize index)
{
    assert(ISCONTAINER(jsonType(container)));
    if (index < 0) {
        return NULL;
    }
    struct JsonCursor c;
    jsonCursorInit(container, kCursorNormal, &c);
    for (JsonSize i = 0; i < index; ++i) {
        jsonCursorNext(&c);
        if (!jsonCursorIsValid(&c)) {
            return NULL;
        }
    }
    return c.value;
}
