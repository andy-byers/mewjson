// mewjson: a tiny JSON parser
// Distributed under the MIT License (http://opensource.org/licenses/MIT)

#include "mewjson.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Maximum number of nested containers allowed in a JSON document
#ifndef MEWJSON_MAX_DEPTH
#define MEWJSON_MAX_DEPTH 10000
#endif // MEWJSON_MAX_DEPTH

// Number of bytes of stack space to use for tracking nested containers before
// resorting to a heap allocation
#ifndef MEWJSON_STACK_SIZE
#define MEWJSON_STACK_SIZE 512
#endif // MEWJSON_STACK_SIZE

#define END_OF_INPUT '\xFF'
#define TRUE (JsonBool)1
#define FALSE (JsonBool)0

#define UNUSED(x) (void)(x)
#define SIZEOF(x) (JsonSize)sizeof(x)
#define COUNTOF(a) (SIZEOF(a) / SIZEOF(*(a)))

#define JSON_MALLOC(a, size) (a).malloc((size_t)(size))
#define JSON_REALLOC(a, ptr, size) (a).realloc(ptr, (size_t)(size))
#define JSON_FREE(a, ptr) (a).free(ptr)

JsonBool defaultAcceptKey(void *ctx, const char *key, JsonSize value)
{
    UNUSED(ctx);
    UNUSED(key);
    UNUSED(value);
    return TRUE;
}

JsonBool defaultAcceptValue(void *ctx, const JsonValue *value)
{
    UNUSED(ctx);
    UNUSED(value);
    return TRUE;
}

JsonBool defaultBeginOrEndContainer(void *ctx, JsonBool isObject)
{
    UNUSED(ctx);
    UNUSED(isObject);
    return TRUE;
}

void jsonParserInit(struct JsonParser *parser, struct JsonHandler *h, struct JsonAllocator *a)
{
    static const struct JsonHandler kHandlerPrototype = {
        .acceptKey = defaultAcceptKey,
        .acceptValue = defaultAcceptValue,
        .beginContainer = defaultBeginOrEndContainer,
        .endContainer = defaultBeginOrEndContainer,
    };
    static const struct JsonAllocator kAllocatorPrototype = {
        .malloc = malloc,
        .realloc = realloc,
        .free = free,
    };
    parser->h = h ? *h : kHandlerPrototype;
    parser->a = a ? *a : kAllocatorPrototype;
}

struct JsonValue {
    // Type of JSON value
    enum JsonType type;

    // Number of bytes in a string, 0 for all other types
    JsonSize size;

    // Decoded data associated with the value
    union {
        const char *string;
        int64_t integer;
        double real;
        JsonBool boolean;
    } v;
};

// State variables used during a parse
struct ParseState {
    // Copies of the handler and allocator held by the JsonParser instance
    struct JsonHandler h;
    struct JsonAllocator a;

    JsonValue value;
    JsonSize offset;

    // Status code pertaining to the parse. Value is kStatusOk on success, kStatusStopped if
    // the parser was stopped, and some other value on failure.
    enum JsonStatus status;

    // Stack of nested container types.
    uint8_t *stack;
    JsonSize depth;
    JsonSize capacity;

    // Array used as backing for the stack until it gets too large.
    uint8_t small[MEWJSON_STACK_SIZE];
};

// Constants related to the character class lookup table
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

// Check for inclusion in one of the character classes
#define ISSPACE(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsSpace)
#define ISFPCHAR(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsFp)
#define ISNUMERIC(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsNumeric)
#define ISHEX(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsHex)

// Get the integer representation of a numeric or hex digit
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

static enum Token scanNull(struct ParseState *s, const char **ptr)
{
    if (getChar(ptr) == 'u' && //
        getChar(ptr) == 'l' && //
        getChar(ptr) == 'l') { //
        s->value = (JsonValue){.type = kTypeNull};
        return kTokenValue;
    }
    s->status = kStatusLiteralInvalid;
    return kTokenError;
}

static enum Token scanTrue(struct ParseState *s, const char **ptr)
{
    if (getChar(ptr) == 'r' && //
        getChar(ptr) == 'u' && //
        getChar(ptr) == 'e') { //
        s->value = (JsonValue){.type = kTypeBoolean, .v = {.boolean = 1}};
        return kTokenValue;
    }
    s->status = kStatusLiteralInvalid;
    return kTokenError;
}

static enum Token scanFalse(struct ParseState *s, const char **ptr)
{
    if (getChar(ptr) == 'a' && //
        getChar(ptr) == 'l' && //
        getChar(ptr) == 's' && //
        getChar(ptr) == 'e') { //
        s->value = (JsonValue){.type = kTypeBoolean, .v = {.boolean = 0}};
        return kTokenValue;
    }
    s->status = kStatusLiteralInvalid;
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

static enum Token scanString(struct ParseState *s, const char **ptr)
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
                            s->status = kStatusStringInvalidCodepoint;
                            return kTokenError;
                        }
                        if (0xD800 <= codepoint && codepoint <= 0xDFFF) {
                            // Codepoint is part of a surrogate pair. Expect a high surrogate
                            // (U+D800–U+DBFF) followed by a low surrogate (U+DC00–U+DFFF).
                            if (codepoint <= 0xDBFF) {
                                if (getChar(ptr) != '\\' || getChar(ptr) != 'u') {
                                    s->status = kStatusStringInvalidCodepoint;
                                    return kTokenError;
                                }
                                const int codepoint2 = getCodepoint(ptr);
                                if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                                    s->status = kStatusStringMissingLowSurrogate;
                                    return kTokenError;
                                }
                                codepoint = (((codepoint - 0xD800) << 10) | (codepoint2 - 0xDC00)) +
                                            0x10000;
                            } else {
                                s->status = kStatusStringMissingHighSurrogate;
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
                        s->status = kStatusStringInvalidEscape;
                        return kTokenError;
                }
                break;
            case '"': {
                // Closing double quote finishes the string.
                s->value = (JsonValue){
                    .type = kTypeString,
                    .size = out - begin,
                    .v = {.string = begin},
                };
                return kTokenString;
            }
            default:
                if ((uint8_t)c < 0x20) {
                    s->status = kStatusStringUnescapedControl;
                    return kTokenError;
                } else if ((uint8_t)c < 0x80) {
                    *out++ = c;
                } else if (scanUtf8(ptr, &out, c)) {
                    s->status = kStatusStringInvalidUtf8;
                    return kTokenError;
                }
        }
    }
}

static enum Token scanReal(struct ParseState *s, const char **ptr, JsonBool isNegative)
{
    const char *begin = *ptr - isNegative;
    // According to RFC 8259, the ABNF for a number looks like:
    //     [ minus ] int [ frac ] [ exp ]
    // We have already parsed the "[ minus ]" and "int" parts. Either the "int" part overflowed, or
    // there exists a "frac" or "exp" part that needs to be scanned. Since parsing floating-point
    // numbers is extremely complicated, we are using strtod(). All we have to do is make sure the
    // rest of the number is formatted correctly, and leave the cursor on the first character that
    // is not part of the number.

    // If an integer overflowed, then there might be some digits left. Skip any remaining
    // digits in the integral part.
    while (ISNUMERIC(peekChar(ptr))) {
        getChar(ptr);
    }

    char c = peekChar(ptr);
    if (c == '.') {
        getChar(ptr);
        // Consume the fractional part.
        if (!ISNUMERIC(peekChar(ptr))) {
            s->status = kStatusNumberMissingFraction;
            return kTokenError;
        }
        while (ISNUMERIC(peekChar(ptr))) {
            getChar(ptr); // Skip digits
        }
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
            s->status = kStatusNumberMissingExponent;
            return kTokenError;
        }
        while (ISNUMERIC(peekChar(ptr))) {
            getChar(ptr); // Skip digits
        }
    }
    // Input has been padded with at least 1 byte that strtod() will not recognize as being part
    // of a real number. Otherwise, strtod() might continue reading past the end of the buffer.
    const double real = strtod(begin, NULL);
    s->value = (JsonValue){.type = kTypeReal, .v = {.real = real}};
    return kTokenValue;
}

static enum Token scanNumber(struct ParseState *s, const char **ptr, JsonBool isNegative)
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
        // Logic in this part was modified from SQLite's JSON module.
        const uint32_t v = NUMVAL(getChar(ptr));
        if (value > INT64_MAX / 10) {
            // This number is definitely too large to be represented as an int64_t (the
            // "value * 10" below will cause signed integer overflow. Parse it as a double
            // instead.
            goto call_scan_real;
        } else if (value == INT64_MAX / 10) {
            // This number might be too large (the "+ v" below might cause an overflow).
            // Handle the special cases. Note that INT64_MAX is equal to 9223372036854775807.
            // In this branch, v is referring to the least-significant digit of INT64_MAX,
            // since the value * 10 below will yield 9223372036854775800.
            if (v == 9) {
                goto call_scan_real;
            } else if (v == 8) {
                // If the number isn't exactly INT64_MIN, then it will overflow and must be
                // parsed as a real.
                if (isNegative && !ISNUMERIC(peekChar(ptr))) {
                    s->value = (JsonValue){.type = kTypeInteger, .v = {.integer = INT64_MIN}};
                    return kTokenValue;
                }
                goto call_scan_real;
            }
        }
        value = value * 10 + v;
    }

    if (!ISFPCHAR(peekChar(ptr))) {
        assert(value != INT64_MIN);
        value = isNegative ? -value : value;
        s->value = (JsonValue){.type = kTypeInteger, .v = {.integer = value}};
        return kTokenValue;
    }

call_scan_real:
    *ptr = begin;
    return scanReal(s, ptr, isNegative);
return_with_error:
    s->status = kStatusNumberInvalid;
    return kTokenError;
}

static enum Token scanToken(struct ParseState *s, const char **ptr)
{
    // skipWhitespace(parser) returns the first non-whitespace character following the last
    // token we read.
    switch (skipWhitespace(ptr)) {
        case '"':
            return scanString(s, ptr);
        case '-':
            return scanNumber(s, ptr, 1);
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
            ungetChar(ptr); // Let scanNumber() read the first digit
            return scanNumber(s, ptr, 0);
        case 'n':
            return scanNull(s, ptr);
        case 't':
            return scanTrue(s, ptr);
        case 'f':
            return scanFalse(s, ptr);
        case ':':
            return kTokenNameSeparator;
        case ',':
            return kTokenValueSeparator;
        case '{':
            return kTokenBeginObject;
        case '[':
            return kTokenBeginArray;
        case '}':
            return kTokenEndObject;
        case ']':
            return kTokenEndArray;
        case END_OF_INPUT:
            return kTokenEndOfInput;
        default:
            s->status = kStatusSyntaxError;
            return kTokenError;
    }
}

// States for the parser
enum State {
    kStateError,
    kStateStop,
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
            /* stp */ {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR}, // sink
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
static int maybeGrowStack(struct ParseState *s)
{
    struct JsonAllocator *a = &s->a;
    if (s->depth >= s->capacity) {
        if (s->capacity == 0) {
            s->capacity = MEWJSON_STACK_SIZE;
            s->stack = s->small;
            return 0;
        }
        JsonSize cap = 4;
        while (cap <= s->depth) {
            cap *= 2;
        }
        uint8_t *stack = JSON_MALLOC(*a, cap);
        if (!stack) {
            s->status = kStatusNoMemory;
            return -1;
        }
        memcpy(stack, s->stack, (size_t)s->depth);
        if (s->stack != s->small) {
            JSON_FREE(*a, s->stack);
        }
        s->stack = stack;
        s->capacity = cap;
    }
    return 0;
}

MEWJSON_NODISCARD
static int parserBeginContainer(struct ParseState *s, JsonBool isObject)
{
    if (s->depth >= MEWJSON_MAX_DEPTH) {
        s->status = kStatusExceededMaxDepth;
        return -1;
    }
    if (maybeGrowStack(s)) {
        return -1;
    }
    s->stack[s->depth] = isObject;
    ++s->depth;
    return 0;
}

MEWJSON_NODISCARD
static enum State parserEndContainer(struct ParseState *s)
{
    struct JsonHandler *h = &s->h;
    // This precondition is ensured by the state transition table in predictState(). This
    // function will never be called unless parserBeginContainer() was called at some point.
    assert(s->depth > 0);
    --s->depth;
    if (!h->endContainer(h->ctx, s->stack[s->depth])) {
        return kStateStop;
    }
    if (s->depth > 0) {
        // Pretend like the parser just finished reading an object member value or an array
        // element. Essentially, we are considering the whole object or array that we just
        // left to be a single value, rather than a compound structure. We have to check
        // the grandparent, because the kOE token has not been accepted yet (acceptToken()
        // below). The parser control is technically still inside the object or array
        // presently being closed.
        return s->stack[s->depth - 1] ? kO2 : kA1;
    }
    // Just closed the root-level structure, so we must be finished. It is an error
    // otherwise, since RFC 8259 only allows a singular root.
    return kStateEnd;
}

static enum State parserAcceptKey(struct ParseState *s)
{
    struct JsonHandler *h = &s->h;
    JsonValue *v = &s->value;
    const JsonBool rc = h->acceptKey(h->ctx, v->v.string, v->size);
    return rc ? kO1 : kStateStop;
}

static enum State parserAcceptValue(struct ParseState *s, enum State dst)
{
    struct JsonHandler *h = &s->h;
    JsonValue *value = &s->value;
    JsonBool rc;
    switch (dst) {
        case kOB:
        case kAB:
            rc = h->beginContainer(h->ctx, dst == kOB);
            break;
        default:
            rc = h->acceptValue(h->ctx, value);
    }
    return rc ? dst : kStateStop;
}

// Transition into the next state
static enum State transitState(struct ParseState *s, enum State dst)
{
    // `dst` is the next state as predicted by predictState(), upon reading token `token`.
    // We need to make sure that the destination state is not a transient state (either
    // kOE or kAE). As described in predictState(), kOE (object end) and kAE (array end) are
    // "pop" states, meaning we pop an element off the stack and leave a nested container.
    // After doing so, we examine the new stack top. Using that value, we transition to
    // either kO2 (object member value) or kA1 (array element). Normally, these states
    // are entered when we have just read an object member value or array element,
    // respectively, so basically, we are just treating the while object or array as a child
    // member/element in its parent object/array.
    switch (dst) {
        case kO1:
            // Indicate that the current node is an object member key.
            return parserAcceptKey(s);
        case kOE:
        case kAE:
            // Leaving a nested container. Correct the state based on the type of container that
            // the parser control has returned into.
            return parserEndContainer(s);
        case kV1:
        case kA1:
        case kO2:
            // Found a JSON value.
            break;
        case kOB:
        case kAB:
            // Entered an object or array.
            if (parserBeginContainer(s, dst == kOB)) {
                return kStateError;
            }
            break;
        default:
            return dst;
    }
    return parserAcceptValue(s, dst);
}

static enum Token parserAdvance(struct ParseState *s, const char **ptr)
{
    if (s->status == kStatusOk) {
        return scanToken(s, ptr);
    }
    return kTokenError;
}

static int parserFinish(struct ParseState *s, enum State state, const char **ptr)
{
    if (ISSPACE(peekChar(ptr))) {
        skipWhitespace(ptr);
    }
    if (state == kStateStop) {
        s->status = kStatusStopped;
        return 0;
    }
    if (s->status != kStatusOk) {
        return -1;
    }
    if (s->depth != 0) {
        // The state transition mechanism prevents the depth from ever getting below 0. Once
        // the last container has been closed, the parser transitions into the end state, a
        // sink state, and refuses to accept any more tokens.
        assert(s->depth > 0);
        s->status = kStatusContainerNotClosed;
        return -1;
    }
    if (state != kStateEnd) {
        // Non-specific syntax error.
        s->status = kStatusSyntaxError;
        return -1;
    }
    return 0;
}

static int parserParse(struct ParseState *s, const char **ptr)
{
    enum State state = kStateBegin;
    enum Token token;
    do {
        token = parserAdvance(s, ptr);
        state = predictState(state, token);
        state = transitState(s, state);
    } while (state > kStateBegin);
    return parserFinish(s, state, ptr);
}

static void parserFinalize(struct ParseState *s, JsonSize offset, JsonSize length)
{
    s->offset = offset < length ? offset : length;
    if (s->status != kStatusOk) {
        return;
    }
    if (offset < length) {
        s->status = kStatusSyntaxError;
    }
}

// Input buffer gets padded with this number of bytes
#define PADDING_LENGTH 4

MEWJSON_NODISCARD
enum JsonStatus jsonParse(const char *input, JsonSize length, struct JsonParser *parser)
{
    struct JsonHandler h = parser->h;
    struct JsonAllocator a = parser->a;
    struct ParseState s = {.h = h, .a = a};
    parser->offset = -1;

    // Allocate a padded buffer to hold the input text. This serves several purposes:
    // 1. Allows scanReal() to call strtod() directly on the input buffer, even if the real number
    //    being parsed is adjacent to the end of the buffer (original buffer is not necessarily
    //    null-terminated).
    // 2. Allows strings to be escaped in-place. This works because an unescaped string is always
    //    shorter than an escaped string.
    // 3. Eliminates an extra check to see if we have reached the end of the buffer before reading
    //    the next character. The parser already switches on the next character, so we just emit a
    //    kTokenEndOfInput token when we encounter an EOF byte. Of course, we also need to check to
    //    make sure we have reached the end of the buffer to handle embedded EOFs, but that happens
    //    just once, at the end.
    char *text = JSON_MALLOC(a, length + PADDING_LENGTH);
    if (!text) {
        return kStatusNoMemory;
    }
    memcpy(text, input, (size_t)length);
    memset(text + length, END_OF_INPUT, PADDING_LENGTH);

    // Parse the document.
    const char *ptr = text;
    parserParse(&s, &ptr);
    parserFinalize(&s, ptr - text, length);

    // Save the offset that the parser left off on and cleanup.
    parser->offset = s.offset;
    parser->status = s.status;
    if (s.stack != s.small) {
        JSON_FREE(a, s.stack);
    }
    JSON_FREE(a, text);
    return s.status;
}

enum JsonType jsonType(const JsonValue *value)
{
    return value->type;
}

JsonSize jsonLength(const JsonValue *value)
{
    return value->size;
}

const char *jsonString(const JsonValue *value)
{
    assert(jsonType(value) == kTypeString);
    return value->v.string;
}

int64_t jsonInteger(const JsonValue *value)
{
    assert(jsonType(value) == kTypeInteger);
    return value->v.integer;
}

double jsonReal(const JsonValue *value)
{
    assert(jsonType(value) == kTypeReal);
    return value->v.real;
}

JsonBool jsonBoolean(const JsonValue *value)
{
    assert(jsonType(value) == kTypeBoolean);
    return value->v.boolean;
}

struct Writer {
    char *ptr;
    JsonSize len;
    JsonSize acc;
    JsonBool sep;
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

// This function seems to generate nicer assembly than calling appendChar() 4 times.
static void appendChar4(struct Writer *w, const char *c4)
{
    w->acc += 4;
    if (w->len >= 4) {
        *w->ptr++ = *c4++;
        *w->ptr++ = *c4++;
        *w->ptr++ = *c4++;
        *w->ptr++ = *c4++;
        w->len -= 4;
    }
}

static void appendString(struct Writer *w, const char *str, JsonSize len)
{
    w->acc += len;
    if (len > 0 && len <= w->len) {
        memcpy(w->ptr, str, (size_t)len);
        w->ptr += len;
        w->len -= len;
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
            // TODO: Add a test that covers this (may need to consult the current locale)
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
    appendString(w, buf, len);
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
    appendString(w, ptr, (JsonSize)(end - ptr));
}

// Modified from SQLite (src/json.c).
static void appendEscapedString(struct Writer *w, const char *str, JsonSize len)
{
    appendChar(w, '"');
    for (JsonSize i = 0; i < len; i++) {
        uint8_t c = ((uint8_t *)str)[i];
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
            appendChar4(w, "\\u00");
            appendChar(w, (char)('0' + (c >> 4)));
            c = (uint8_t) "0123456789abcdef"[c & 0xF];
        }
        appendChar(w, (char)c);
    }
    appendChar(w, '"');
}

static void testWriteValue(struct Writer *w, const JsonValue *value)
{
    switch (jsonType(value)) {
        case kTypeString:
            appendEscapedString(w, jsonString(value), jsonLength(value));
            break;
        case kTypeInteger:
            appendInteger(w, jsonInteger(value));
            break;
        case kTypeBoolean:
            if (jsonBoolean(value)) {
                appendChar4(w, "true");
            } else {
                appendChar4(w, "fals");
                appendChar(w, 'e');
            }
            break;
        case kTypeReal:
            if (isfinite(jsonReal(value))) {
                appendFiniteReal(w, jsonReal(value));
                break;
            }
            // Intentional fallthrough: convert +/-Inf into null.
        default: // kTypeNull
            appendChar4(w, "null");
            break;
    }
}

static void minifyMaybeAddComma(struct Writer *w, JsonBool sepAfter)
{
    if (w->sep) {
        appendChar(w, ',');
    }
    w->sep = sepAfter;
}

static JsonBool minifyAcceptKey(void *ctx, const char *key, JsonSize length)
{
    struct Writer *w = ctx;
    minifyMaybeAddComma(w, FALSE);
    appendEscapedString(w, key, length);
    appendChar(w, ':');
    return TRUE;
}

static JsonBool minifyAcceptValue(void *ctx, const JsonValue *value)
{
    struct Writer *w = ctx;
    minifyMaybeAddComma(w, TRUE);
    testWriteValue(w, value);
    return TRUE;
}

static JsonBool minifyBeginContainer(void *ctx, JsonBool isObject)
{
    struct Writer *w = ctx;
    minifyMaybeAddComma(w, FALSE);
    if (isObject) {
        appendChar(w, '{');
    } else {
        appendChar(w, '[');
    }
    return TRUE;
}

static JsonBool minifyEndContainer(void *ctx, JsonBool isObject)
{
    struct Writer *w = ctx;
    if (isObject) {
        appendChar(w, '}');
    } else {
        appendChar(w, ']');
    }
    w->sep = TRUE;
    return TRUE;
}

MEWJSON_NODISCARD
JsonSize jsonMinify(const char *input, JsonSize inputLen, char *output, JsonSize outputLen,
                    struct JsonParser *parser)
{
    struct Writer w = {.ptr = output, .len = outputLen};
    struct JsonHandler userH = parser->h; // Save the user handler
    parser->h = (struct JsonHandler){
        .ctx = &w,
        .acceptKey = minifyAcceptKey,
        .acceptValue = minifyAcceptValue,
        .beginContainer = minifyBeginContainer,
        .endContainer = minifyEndContainer,
    };
    const enum JsonStatus s = jsonParse(input, inputLen, parser);
    parser->h = userH; // Restore the user handler
    return s == kStatusOk ? w.acc : -1;
}
