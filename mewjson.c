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

// Number of nested containers to push onto an internal stack before resorting to a heap
// allocation
// Each stack element is the size of a pointer.
#ifndef MEWJSON_STACK_DEPTH
#define MEWJSON_STACK_DEPTH 512
#endif // MEWJSON_STACK_DEPTH

#define END_OF_INPUT '\x00'
#define TRUE (JsonBool)1
#define FALSE (JsonBool)0

#define UNUSED(x) (void)(x)
#define SIZEOF(x) (JsonSize)sizeof(x)
#define COUNTOF(a) (SIZEOF(a) / SIZEOF(*(a)))

// Preconditions that must be met for the binary encoding to work properly
_Static_assert(SIZEOF(double) == SIZEOF(uint64_t), "SIZEOF(double) must equal SIZEOF(uint64_t)");

#define JSON_MALLOC(a, n) (a).malloc((a).ctx, (size_t)(n))
#define JSON_REALLOC(a, p, n0, n1) (a).realloc((a).ctx, p, (size_t)(n0), (size_t)(n1))
#define JSON_FREE(a, p, n) (a).free((a).ctx, p, (size_t)(n))

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define MAX_VARINT_SIZE 10

static JsonSize varintLengthU64(uint64_t v)
{
    JsonSize len = 1;
    while (v >= 128) {
        v >>= 7;
        len++;
    }
    return len;
}

static uint8_t *encodeVarintU64(uint8_t *dst, uint64_t v)
{
    static const uint64_t kMsb = 0x80;
    while (v >= kMsb) {
        *dst++ = v | kMsb;
        v >>= 7;
    }
    *dst++ = (uint8_t)v;
    return dst;
}

static const uint8_t *decodeVarintU64(const uint8_t *p, uint64_t *value)
{
    uint64_t result = 0;
    for (uint32_t shift = 0; shift <= 63; shift += 7) {
        uint64_t byte = *p;
        p++;
        if (byte & 128) {
            result |= ((byte & 127) << shift);
        } else {
            result |= (byte << shift);
            break;
        }
    }
    *value = result;
    return p;
}

static JsonSize varintLengthI64(int64_t v)
{
    uint64_t u = (uint64_t)v << 1;
    return varintLengthU64(v < 0 ? ~u : u);
}

static uint8_t *encodeVarintI64(uint8_t *dst, int64_t v)
{
    uint64_t u = (uint64_t)v << 1;
    return encodeVarintU64(dst, v < 0 ? ~u : u);
}

static const uint8_t *decodeVarintI64(const uint8_t *p, int64_t *value)
{
    uint64_t u;
    p = decodeVarintU64(p, &u);
    int64_t v = (int64_t)(u >> 1);
    *value = u & 1 ? ~v : v;
    return p;
}

static void *jsonMalloc(void *ctx, size_t size)
{
    UNUSED(ctx);
    UNUSED(size);
    return malloc(size);
}

static void *jsonRealloc(void *ctx, void *ptr, size_t oldSize, size_t newSize)
{
    UNUSED(ctx);
    UNUSED(oldSize);
    return realloc(ptr, newSize);
}

static void jsonFree(void *ctx, void *ptr, size_t size)
{
    UNUSED(ctx);
    UNUSED(size);
    free(ptr);
}

void jsonParserInit(struct JsonParser *parser, struct JsonAllocator *a)
{
    static const struct JsonAllocator kAllocatorPrototype = {
        .malloc = jsonMalloc,
        .realloc = jsonRealloc,
        .free = jsonFree,
    };
    parser->a = a ? *a : kAllocatorPrototype;
}

enum {
    kTypeWidth = 4,
    kTypeMask = 0x0F,
    kBooleanMask = 0x80,
};

struct UnpackNode {
    // Type of JSON value
    int type;

    // Number of bytes occupied by the node and all of its children, if any
    JsonSize size;

    // Decoded data associated with the node
    union {
        const char *string;
        int64_t integer;
        double real;
        JsonBool boolean;
    };
};

#define ISCONTAINER(t) ((t) >= kTypeObject)

static uint8_t *containerDecodeSize(const JsonValue *value, JsonSize *size)
{
    assert(ISCONTAINER(jsonType(value)));
    uint8_t *ptr = (uint8_t *)value + 1;

    int64_t i;
    decodeVarintI64(ptr, &i);
    *size = i;
    return ptr + (ptr[-1] >> kTypeWidth);
}

static uint8_t *containerDecodeUp(const JsonValue *value, JsonSize *up)
{
    assert(ISCONTAINER(jsonType(value)));
    uint8_t *ptr = (uint8_t *)value + 1;
    ptr += ptr[-1] >> kTypeWidth;

    uint64_t u;
    ptr = (uint8_t *)decodeVarintU64(ptr, &u);
    *up = (JsonSize)u;
    return ptr;
}

static JsonSize sizeOfContainer(const JsonValue *value)
{
    assert(ISCONTAINER(jsonType(value)));
    uint8_t *ptr = (uint8_t *)value + 1;
    ptr += ptr[-1] >> kTypeWidth;
    while (*ptr++ & 0x80) {
        // Skip varint
    }
    return ptr - (uint8_t *)value;
}

static JsonSize isContainerEmpty(const JsonValue *value)
{
    assert(ISCONTAINER(jsonType(value)));
    JsonSize size;
    containerDecodeSize(value, &size);
    return size == sizeOfContainer(value);
}

static JsonSize sizeOfScalar(const JsonValue *value)
{
    const uint8_t *ptr = (uint8_t *)value;
    int type = *ptr++ & kTypeMask;
    assert(!ISCONTAINER(type));
    switch (type) {
        case kTypeString: {
            int64_t length;
            ptr = decodeVarintI64(ptr, &length);
            ptr += length;
            break;
        }
        case kTypeReal:
        case kTypeInteger:
            while (*ptr++ & 0x80) {
                // Skip varint
            }
        default: // kTypeBoolean || kTypeNull
            break;
    }
    return ptr - (uint8_t *)value;
}

static JsonSize sizeOfSubTree(const JsonValue *value)
{
    if (ISCONTAINER(jsonType(value))) {
        JsonSize size;
        containerDecodeSize(value, &size);
        return size;
    }
    return sizeOfScalar(value);
}

static JsonBool isObjectNode(const JsonValue *v)
{
    return jsonType(v) == kTypeObject;
}

// Represents a JSON parse tree
struct JsonDocument {
    struct JsonAllocator a;

    // Buffer of packed JSON value nodes
    char *buffer;
    JsonSize length;
    JsonSize capacity;
};

// State variables used during a parse
struct ParseState {
    struct JsonAllocator a;
    JsonDocument *doc;
    const char *end;

    JsonSize depth;
    JsonSize offset;
    JsonSize upIndex;

    // Status code pertaining to the parse. Value is kStatusOk on success, kStatusStopped if
    // the parser was stopped, and some other value on failure.
    enum JsonStatus status;
};

static uint8_t *docWriteNode(struct ParseState *s, int type, JsonSize fixedPayload)
{
    JsonDocument *doc = s->doc;
    static const JsonSize kExtraSize = 1 + MAX_VARINT_SIZE;
    // Determine the largest amount of memory the next node could possibly need.
    const JsonSize maxLength = doc->length + fixedPayload + kExtraSize;
    if (maxLength > doc->capacity) {
        JsonSize cap = 4;
        while (cap < maxLength) {
            cap *= 2;
        }
        // Resize the buffer to fit cap bytes.
        char *ptr = JSON_REALLOC(s->doc->a, doc->buffer, doc->capacity, cap);
        if (!ptr) {
            s->status = kStatusNoMemory;
            return NULL;
        }
        doc->buffer = ptr;
        doc->capacity = cap;
    }
    uint8_t *ptr = (uint8_t *)&doc->buffer[doc->length];
    // Account for the type and fixed payload.
    doc->length += fixedPayload + 1;
    *ptr++ = (uint8_t)type;
    return ptr;
}

static void docAppendContainer(struct ParseState *s, JsonBool isObject, JsonSize up,
                               JsonSize maxText)
{
    assert(up >= 0);
    // maxHdr is the maximum number of bytes needed to store the number of bytes contained in a
    // container node. This is a bit wasteful for most containers, but it prevents us from having
    // to shift elements around or look ahead for the end of a container before writing its header.
    // The number passed to varintLengthI64() is the maximum number of bytes that the root container
    // node can contain, given the text length. Pretend that the root is an array filled with real
    // numbers, all equal to 1.0. The real number 1.0 happens to encode to a 10-byte varint, with
    // 1 byte required to hold the type. This is the worst possible case.
    const JsonSize maxHdr = varintLengthI64((-2 /* [] */ + maxText + 3 /* round up */) / 4 * 11);
    uint8_t *ptr = docWriteNode(s, isObject ? kTypeObject : kTypeArray, maxHdr);
    if (ptr) {
        JsonDocument *doc = s->doc;
        // Indicate how many bytes are allotted to the size.
        ptr[-1] |= maxHdr << kTypeWidth;
        // Encode the back reference.
        ptr += maxHdr;
        uint8_t *end = encodeVarintU64(ptr, (uint64_t)up);
        doc->length += end - ptr; // Just the varint size
    }
}

static void docAppendString(struct ParseState *s, const char *string, JsonSize length)
{
    uint8_t *ptr = docWriteNode(s, kTypeString, length);
    if (ptr) {
        uint8_t *end = encodeVarintI64(ptr, length);
        memcpy(end, string, (size_t)length);
        s->doc->length += end - ptr;
    }
}

static void docAppendInteger(struct ParseState *s, int64_t i)
{
    uint8_t *ptr = docWriteNode(s, kTypeInteger, 0);
    if (ptr) {
        uint8_t *end = encodeVarintI64(ptr, i);
        s->doc->length += end - ptr;
    }
}

static void docAppendReal(struct ParseState *s, double r)
{
    uint8_t *ptr = docWriteNode(s, kTypeReal, 0);
    if (ptr) {
        uint64_t u;
        memcpy(&u, &r, sizeof(r));
        uint8_t *end = encodeVarintU64(ptr, u);
        s->doc->length += end - ptr;
    }
}

static void docAppendBoolean(struct ParseState *s, JsonBool b)
{
    uint8_t *ptr = docWriteNode(s, kTypeBoolean, 0);
    if (ptr && b) {
        ptr[-1] |= kBooleanMask;
    }
}

static void docAppendNull(struct ParseState *s)
{
    docWriteNode(s, kTypeNull, 0);
}

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
    while (ISSPACE(c = peekChar(ptr))) {
        getChar(ptr);
    }
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
        docAppendNull(s);
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
        docAppendBoolean(s, 1);
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
        docAppendBoolean(s, 0);
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
        out += (n);                                                                                \
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
        out += 4;
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
        docAppendString(s, begin, out - begin);
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
    docAppendReal(s, real);
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
                    docAppendInteger(s, INT64_MIN);
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
        docAppendInteger(s, value);
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
    if (skipWhitespace(ptr) == END_OF_INPUT) {
        return kTokenEndOfInput;
    }
    switch (getChar(ptr)) {
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
static int parserBeginContainer(struct ParseState *s, JsonSize maxText, JsonBool isObject)
{
    ++s->depth;
    if (s->depth > MEWJSON_MAX_DEPTH) {
        s->status = kStatusExceededMaxDepth;
        return -1;
    }
    const JsonSize up = s->upIndex;
    s->upIndex = s->doc->length;
    docAppendContainer(s, isObject, s->doc->length - up, maxText);
    return 0;
}

MEWJSON_NODISCARD
static enum State parserEndContainer(struct ParseState *s)
{
    // This precondition is ensured by the state transition table in predictState(). This
    // function will never be called unless parserBeginContainer() was called at some point.
    assert(s->depth > 0);
    JsonDocument *doc = s->doc;
    --s->depth;

    // Write the container size in bytes right after the type.
    uint8_t *hdr = (uint8_t *)&doc->buffer[s->upIndex + 1];
    const JsonSize maxHdr = hdr[-1] >> kTypeWidth;
    encodeVarintI64(hdr, doc->length - s->upIndex);
    hdr += maxHdr;

    uint64_t up;
    decodeVarintU64(hdr, &up);
    s->upIndex -= (JsonSize)up;

    if (s->depth > 0) {
        hdr = (uint8_t *)&doc->buffer[s->upIndex];
        // Pretend like the parser just finished reading an object member value or an array
        // element. Essentially, we are considering the whole object or array that we just
        // left to be a single value, rather than a compound structure. We have to check
        // the grandparent, because the kOE token has not been accepted yet (acceptToken()
        // below). The parser control is technically still inside the object or array
        // presently being closed.
        return (hdr[0] & kTypeMask) == kTypeObject ? kO2 : kA1;
    }
    // Just closed the root-level structure, so we must be finished. It is an error
    // otherwise, since RFC 8259 only allows a singular root.
    return kStateEnd;
}

// Transition into the next state
static enum State transitState(struct ParseState *s, enum State dst, const char *ptr)
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
        case kOE:
        case kAE:
            // Leaving a nested container. Correct the state based on the type of container that
            // the parser control has returned into.
            return parserEndContainer(s);
        case kO1:
        case kV1:
        case kA1:
        case kO2:
            // Found a JSON key or value.
            break;
        case kOB:
        case kAB:
            // Entered an object or array.
            if (parserBeginContainer(s, s->end - ptr, dst == kOB)) {
                return kStateError;
            }
            break;
        default:
            break;
    }
    return dst;
}

static enum Token parserAdvance(struct ParseState *s, const char **ptr)
{
    if (s->status == kStatusOk) {
        return scanToken(s, ptr);
    }
    return kTokenError;
}

static int parserFinish(struct ParseState *s, enum State state, JsonSize offset, JsonSize length)
{
    s->offset = offset < length ? offset : length;
    if (s->status != kStatusOk) {
        return -1;
    }
    if (state == kStateStop) {
        s->status = kStatusStopped;
        return 0;
    }
    if (offset < length) {
        s->status = kStatusSyntaxError;
        return -1;
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

static int parserParse(struct ParseState *s, const char **ptr, JsonSize length)
{
    const char *begin = *ptr;
    enum State state = kStateBegin;
    enum Token token;
    do {
        token = parserAdvance(s, ptr);
        state = predictState(state, token);
        state = transitState(s, state, *ptr);
    } while (state > kStateBegin);

    // Skip the rest of the whitespace. We need to determine if the parser has made it to the
    // end of the buffer or not. If not, then there must be junk at the end of the input.
    skipWhitespace(ptr);
    return parserFinish(s, state, *ptr - begin, length);
}

MEWJSON_NODISCARD
JsonDocument *createDocAndBuffer(struct JsonAllocator *a, JsonSize length, char **pBuf)
{
    JsonDocument *doc = JSON_MALLOC(*a, SIZEOF(*doc));
    char *buf = JSON_MALLOC(*a, length);
    if (!doc || !buf) {
        JSON_FREE(*a, doc, SIZEOF(*doc));
        JSON_FREE(*a, buf, length);
        return NULL;
    }
    *pBuf = buf;
    return doc;
}

// Input buffer gets padded with this number of bytes
#define PADDING_LENGTH 4

MEWJSON_NODISCARD
JsonDocument *jsonParse(const char *input, JsonSize length, struct JsonParser *parser)
{
    struct JsonAllocator a = parser->a;
    parser->offset = -1;

    // Allocate a padded buffer at the end of the document object to hold the input text. This
    // serves several purposes:
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
    char *text;
    const JsonSize paddedLen = length + PADDING_LENGTH;
    JsonDocument *doc = createDocAndBuffer(&a, paddedLen, &text);
    if (!doc) {
        parser->status = kStatusNoMemory;
        return NULL;
    }
    struct ParseState s = {.doc = doc, .end = text + length};
    *doc = (JsonDocument){.a = a};

    memcpy(text, input, (size_t)length);
    memset(text + length, END_OF_INPUT, PADDING_LENGTH);

    // Parse the document.
    const char *ptr = text;
    parserParse(&s, &ptr, length);
    if (s.status != kStatusOk) {
        jsonDestroyDocument(doc);
        doc = NULL;
    }
    parser->offset = s.offset;
    parser->status = s.status;
    JSON_FREE(a, text, paddedLen);
    return doc;
}

MEWJSON_NODISCARD
JsonDocument *jsonCloneDocument(JsonDocument *doc)
{
    char *buffer;
    const JsonSize length = doc->length;
    JsonDocument *doc2 = createDocAndBuffer(&doc->a, length, &buffer);
    if (doc2) {
        *doc2 = *doc;
        memcpy(buffer, doc->buffer, (size_t)length);
        doc2->capacity = length;
        doc2->buffer = buffer;
    }
    return doc2;
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

static void writeScalar(struct Writer *w, struct JsonCursor *c)
{
    const JsonValue *value = jsonCursorValue(c);
    assert(!ISCONTAINER(jsonType(value)));
    switch (jsonType(value)) {
        case kTypeString: {
            JsonSize length;
            const char *string = jsonString(value, &length);
            appendEscapedString(w, string, length);
            break;
        }
        case kTypeInteger:
            appendInteger(w, jsonInteger(value));
            break;
        case kTypeBoolean:
            if (jsonBoolean(value)) {
                appendString(w, "true", 4);
            } else {
                appendString(w, "false", 5);
            }
            break;
        case kTypeReal:
            if (isfinite(jsonReal(value))) {
                appendFiniteReal(w, jsonReal(value));
                break;
            }
            // Intentional fallthrough
        default: // kTypeNull
            appendString(w, "null", 4);
            break;
    }
}

static void writeEmptyContainer(struct Writer *w, struct JsonCursor *c)
{
    appendString(w, isObjectNode(jsonCursorValue(c)) ? "{}" : "[]", 2);
}

static JsonValue *cursorNext(struct JsonCursor *c)
{
    assert(jsonCursorIsValid(c));
    JsonValue *value = jsonCursorValue(c);
    if (ISCONTAINER(jsonType(value))) {
        c->cursor += sizeOfContainer(value);
    } else {
        c->cursor += sizeOfScalar(value);
    }
    return jsonCursorValue(c);
}

void jsonCursorInit(JsonValue *root, enum JsonCursorMode mode, struct JsonCursor *c)
{
    *c = (struct JsonCursor){
        .root = (char *)root,
        .cursor = (char *)root,
        .total = sizeOfSubTree(root),
        .mode = mode,
    };
    if (mode == kCursorNormal) {
        // Enter a container or invalidate
        cursorNext(c);
    }
}

JsonValue *jsonCursorValue(const struct JsonCursor *c)
{
    return (JsonValue *)c->cursor;
}

JsonValue *jsonCursorParent(const struct JsonCursor *c)
{
    return (JsonValue *)c->parent;
}

JsonBool jsonCursorIsValid(const struct JsonCursor *c)
{
    // Must have called jsonCursorInit(..., c). Also, the cursor must never move past the
    // "1 past the end" element of the buffer. The call to jsonCursorNext(c) that invalidates
    // c should put the cursor exactly 1 past the end.
    assert(c->cursor && c->cursor - c->root <= c->total);
    return c->cursor - c->root < c->total;
}

void jsonCursorNext(struct JsonCursor *c)
{
    assert(jsonCursorIsValid(c));
    if (c->mode == kCursorNormal) {
        // Skip the node, and all of its children if it is a container.
        c->cursor += sizeOfSubTree(jsonCursorValue(c));
    } else {
        JsonValue *value = jsonCursorValue(c);
        cursorNext(c); // Move past the current node
        if (ISCONTAINER(jsonType(value))) {
            c->parent = (char *)value;
        } else if (!c->parent) {
            return;
        }
        // Make sure the parent pointer is correct.
        while (c->parent != c->root) {
            JsonValue *parent = (JsonValue *)c->parent;
            if (c->cursor - c->parent < sizeOfSubTree(parent)) {
                break;
            }
            JsonSize up;
            containerDecodeUp(parent, &up);
            c->parent -= up;
        }
    }
}

MEWJSON_NODISCARD
JsonSize jsonStringify(char *buffer, JsonSize length, JsonValue *root)
{
    struct Writer w = {
        .ptr = buffer,
        .len = length,
    };
    struct JsonCursor c;
    jsonCursorInit(root, kCursorRecursive, &c);
    if (!jsonCursorIsValid(&c)) {
        return -1;
    } else if (!ISCONTAINER(jsonType(root))) {
        // Root is not a container. Write out the single value and return.
        writeScalar(&w, &c);
        return w.acc;
    } else if (isContainerEmpty(root)) {
        // Root container is empty.
        writeEmptyContainer(&w, &c);
        return w.acc;
    }
    for (JsonValue *parent = NULL;;) {
        JsonValue *value = jsonCursorValue(&c);
        if (parent && isObjectNode(parent)) {
            // Write an object key. Advance to the value.
            assert(jsonType(value) == kTypeString);
            JsonSize keyLen;
            const char *key = jsonString(value, &keyLen);
            appendEscapedString(&w, key, keyLen);
            appendChar(&w, ':');
            value = cursorNext(&c);
        }

        if (ISCONTAINER(jsonType(value))) {
            // Value is a nested container.
            if (!isContainerEmpty(value)) {
                appendChar(&w, "[{"[isObjectNode(value)]);
                parent = value;
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
        // NOTE: clang-tidy complains that "parent" might be NULL here, resulting in a null pointer
        //       dereference. The guard before the main loop should prevent that from happening.
        if (c.cursor - (char *)parent < sizeOfSubTree(parent)) {
            appendChar(&w, ',');
        } else {
            appendChar(&w, "]}"[isObjectNode(parent)]);
            // Back out of nested containers. This block doesn't move the cursor at all, it just
            // adjusts the parent pointer until a container is found with children that still need
            // to be written.
            if (parent == root) {
                break;
            }
            JsonSize up;
            containerDecodeUp(parent, &up);
            char *grandparent = (char *)parent - up;
            parent = (JsonValue *)grandparent;
            goto next_iteration;
        }
    }
    return w.acc;
}

enum JsonType jsonType(const JsonValue *value)
{
    const uint8_t *ptr = (uint8_t *)value;
    return ptr[0] & kTypeMask;
}

JsonSize jsonContainerLength(const JsonValue *value)
{
    if (!ISCONTAINER(jsonType(value))) {
        return 0;
    }
    JsonSize length = 0;
    struct JsonCursor c;
    jsonCursorInit((JsonValue *)value, kCursorNormal, &c);
    while (jsonCursorIsValid(&c)) {
        jsonCursorNext(&c);
        ++length;
    }
    return length;
}

const char *jsonString(const JsonValue *value, JsonSize *length)
{
    assert(jsonType(value) == kTypeString);
    const uint8_t *ptr = (uint8_t *)value + 1;

    int64_t size;
    ptr = decodeVarintI64(ptr, &size);
    if (length) {
        *length = size;
    }
    return (const char *)ptr;
}

int64_t jsonInteger(const JsonValue *value)
{
    assert(jsonType(value) == kTypeInteger);
    const uint8_t *ptr = (uint8_t *)value + 1;

    int64_t i;
    decodeVarintI64(ptr, &i);
    return i;
}

double jsonReal(const JsonValue *value)
{
    assert(jsonType(value) == kTypeReal);
    const uint8_t *ptr = (uint8_t *)value + 1;

    uint64_t u;
    decodeVarintU64(ptr, &u);

    double real;
    memcpy(&real, &u, sizeof(real));
    return real;
}

JsonBool jsonBoolean(const JsonValue *value)
{
    assert(jsonType(value) == kTypeBoolean);
    const uint8_t *ptr = (uint8_t *)value;
    return ptr[0] & kBooleanMask;
}

JsonValue *jsonRoot(JsonDocument *doc)
{
    assert(doc && doc->buffer);
    return (JsonValue *)&doc->buffer[0];
}

void jsonDestroyDocument(JsonDocument *doc)
{
    if (doc) {
        JSON_FREE(doc->a, doc->buffer, doc->capacity);
        JSON_FREE(doc->a, doc, SIZEOF(*doc));
    }
}

JsonSize jsonObjectFind(JsonValue *obj, const char *key, JsonSize length)
{
    assert(isObjectNode(obj));
    struct JsonCursor c;
    jsonCursorInit(obj, kCursorNormal, &c);
    for (JsonSize n, i = 0; jsonCursorIsValid(&c); ++i) {
        const char *p = jsonString(jsonCursorValue(&c), &n);
        if (n == length && 0 == memcmp(p, key, (size_t)length)) {
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
    return jsonCursorValue(&c);
}
