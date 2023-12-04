// mewjson: a tiny JSON parser
// Distributed under the MIT License (http://opensource.org/licenses/MIT)

#include "mewjson.h"
#include <assert.h>
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
    };
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
    //                     0bxx xx xx xx
    kMaskIsSpace = 1,     // 00 00 00 01
    kMaskIsFp = 2,        // 00 00 00 10
    kMaskIsNumeric = 4,   // 00 00 01 00
    kMaskIsHex = 8,       // 00 00 10 00
    kMaskIsNonAscii = 16, // 00 01 00 00
    kMaskIsAsciiEnd = 32, // 00 10 00 00
};

// clang-format off
// Character class lookup table: 4 least-significant bits of each byte store flags indicating what
// class the character belongs to: one of "space", "floating-point indicator", "numeric digit", or
// "hex digit". 'e' and 'E' belong to both the "floating-point indicator" and "hex digit" classes.
// The masks and shift values above are used to test for inclusion in one of the classes, and to
// extract the value bits for numeric and hex digits.
static const uint8_t kCharClassTable[256] = {
    32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 32, 32, 33, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     1,  0, 32,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12,  0,  0,  0,  0,  0,  0,
     0,  8,  8,  8,  8, 10,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 32,  0,  0,  0,
     0,  8,  8,  8,  8, 10,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
};

// Check for inclusion in one of the character classes
#define ISSPACE(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsSpace)
#define ISFPCHAR(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsFp)
#define ISNUMERIC(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsNumeric)
#define ISHEX(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsHex)
#define ISASCIIEND(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsAsciiEnd)
#define ISNONASCII(c) (kCharClassTable[(uint8_t)(c)] & kMaskIsNonAscii)

static const uint8_t kNumericTable[256] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
    0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

// Get the integer representation of a numeric or hex digit
#define NUMVAL(c) (kNumericTable[(uint8_t)(c)])
#define HEXVAL(c) (kNumericTable[(uint8_t)(c)])

// clang-format on

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
    if (!ISHEX((*ptr)[0]) || //
        !ISHEX((*ptr)[1]) || //
        !ISHEX((*ptr)[2]) || //
        !ISHEX((*ptr)[3])) { //
        return -1;
    }
    return HEXVAL(getChar(ptr)) << 12 | //
           HEXVAL(getChar(ptr)) << 8 |  //
           HEXVAL(getChar(ptr)) << 4 |  //
           HEXVAL(getChar(ptr));        //
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
        s->value = (JsonValue){.type = kTypeBoolean, .boolean = 1};
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
        s->value = (JsonValue){.type = kTypeBoolean, .boolean = 0};
        return kTokenValue;
    }
    s->status = kStatusLiteralInvalid;
    return kTokenError;
}

// Tables are from https://arxiv.org/pdf/2010.03090.pdf
MEWJSON_NODISCARD
static JsonSize scanUtf8(const char **ptr, char **out)
{
    // clang-format off
    static uint8_t kLookup1[256] = {
         8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
         8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
        10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
        11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
        11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
         8,  8,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
         1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
         4,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  5,  2,  2,
         6,  3,  3,  3,  7,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
    };
    static uint8_t kLookup2[108] = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 1, 1, 1,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 2, 2,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 1,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 1, 1, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 2,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    };
    // clang-format on

    uint32_t state = 0;
    do {
        const uint8_t c = (uint8_t)peekChar(ptr);
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        *out[0] = getChar(ptr);
        ++*out;
    } while (state % 8 != 0);
    return state ? -1 : 0;
}

static enum Token scanString(struct ParseState *s, const char **ptr)
{
    // Cast away constness. *ptr points into heap-allocated memory. This routine unescapes
    // strings inplace, since we have already made a copy of the whole text. The unescaping
    // process only ever decreases the length of a string, so this will always work.
    char *out = (char *)*ptr;
    const char *begin = *ptr;
    char c;

#define CHECK_ASCII(n, body)                                                                       \
    if (ISASCIIEND(c = (*ptr)[n])) {                                                               \
        body;                                                                                      \
        *ptr += (n);                                                                               \
        break;                                                                                     \
    }

    for (;;) {
handle_ascii:
        // This ends up being a very hot loop for JSON documents containing strings. It has been
        // unrolled for better performance.
        CHECK_ASCII(0, {})
        CHECK_ASCII(1, {
            // Copy ASCII byte to unescaped string
            out[0] = (*ptr)[0];
        })
        CHECK_ASCII(2, {
            out[0] = (*ptr)[0];
            out[1] = (*ptr)[1];
        })
        CHECK_ASCII(3, {
            out[0] = (*ptr)[0];
            out[1] = (*ptr)[1];
            out[2] = (*ptr)[2];
        })
        out[0] = (*ptr)[0];
        out[1] = (*ptr)[1];
        out[2] = (*ptr)[2];
        out[3] = (*ptr)[3];
        *ptr += 4;
    }
#undef CHECK_ASCII

    if (c == '\\') {
        ++*ptr;
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
                        codepoint =
                            (((codepoint - 0xD800) << 10) | (codepoint2 - 0xDC00)) + 0x10000;
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
    } else if (c == '"') {
        // Closing double quote finishes the string.
        s->value = (JsonValue){
            .type = kTypeString,
            .size = out - begin,
            .string = begin,
        };
        ++*ptr;
        return kTokenString;
    } else if (ISNONASCII(c)) {
        if (scanUtf8(ptr, &out)) {
            s->status = kStatusStringInvalidUtf8;
            return kTokenError;
        }
    } else {
        s->status = kStatusStringUnescapedControl;
        return kTokenError;
    }
    goto handle_ascii;
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
    s->value = (JsonValue){.type = kTypeReal, .real = real};
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
                    s->value = (JsonValue){.type = kTypeInteger, .integer = INT64_MIN};
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
        s->value = (JsonValue){.type = kTypeInteger, .integer = value};
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
    const JsonBool rc = h->acceptKey(h->ctx, v->string, v->size);
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
    return value->string;
}

int64_t jsonInteger(const JsonValue *value)
{
    assert(jsonType(value) == kTypeInteger);
    return value->integer;
}

double jsonReal(const JsonValue *value)
{
    assert(jsonType(value) == kTypeReal);
    return value->real;
}

JsonBool jsonBoolean(const JsonValue *value)
{
    assert(jsonType(value) == kTypeBoolean);
    return value->boolean;
}
