// mewjson: a tiny JSON parser
// Distributed under the MIT License (http://opensource.org/licenses/MIT)
#ifndef MEWJSON_H
#define MEWJSON_H

#include <stddef.h>
#include <stdint.h>

// Maximum number of nested containers allowed in a JSON document
#ifndef MEWJSON_MAX_DEPTH
#define MEWJSON_MAX_DEPTH 10000
#endif // MEWJSON_MAX_DEPTH

#ifndef MEWJSON_NODISCARD
#if defined(_MSC_VER)
#define MEWJSON_NODISCARD _Check_return_
#elif __has_attribute(warn_unused_result)
#define MEWJSON_NODISCARD __attribute((warn_unused_result))
#else
#define MEWJSON_NODISCARD
#endif
#endif // MEWJSON_NODISCARD

// Primitive types
typedef _Bool JsonBool;
typedef ptrdiff_t JsonSize;

// JSON value (object, array, string, integer, real number, boolean, or null)
typedef struct JsonValue JsonValue;

// Describes the type of JSON value
enum JsonType {
    kTypeString,
    kTypeInteger,
    kTypeReal,
    kTypeBoolean,
    kTypeNull,
    kTypeObject,
    kTypeArray,
};

// Status code produced by the parser
enum JsonParseStatus {
    kParseOk,
    kParseStopped,
    kParseNoMemory,
    kParseExceededMaxDepth,
    kParseStringInvalidEscape,
    kParseStringInvalidCodepoint,
    kParseStringMissingHighSurrogate,
    kParseStringMissingLowSurrogate,
    kParseStringUnescapedControl,
    kParseStringInvalidUtf8,
    kParseLiteralInvalid,
    kParseNumberMissingExponent,
    kParseNumberMissingFraction,
    kParseNumberInvalid,
    kParseContainerNotClosed,
    kParseSyntaxError,
    kParseStatusCount
};

// User-defined callbacks to call as the document is parsed
struct JsonHandler {
    void *ctx;
    JsonBool (*acceptKey)(void *, const char *, JsonSize);
    JsonBool (*acceptValue)(void *, JsonValue *);
    JsonBool (*beginObject)(void *);
    JsonBool (*beginArray)(void *);
    JsonBool (*endObject)(void *);
    JsonBool (*endArray)(void *);
};

// General-purpose allocation routines used by the parser
// User-provided allocators must return memory that is aligned to at least _Alignof(max_align_t).
struct JsonAllocator {
    void *(*malloc)(size_t);
    void *(*realloc)(void *, size_t);
    void (*free)(void *);
};

// JSON parser context
struct JsonParser {
    struct JsonAllocator a;
    struct JsonHandler h;

    // Byte offset that the parser stopped on in the input buffer.
    JsonSize offset;
};

// Initialize a parser
// This function must be called prior to jsonRead(). If the handler or allocator parameters are
// NULL, then default values are used. The default handler does nothing, and the default
// allocator uses the standard C dynamic memory management functions (malloc, realloc, and free).
// If either a handler or allocator is provided, then all function pointers contained within must
// be valid.
void jsonParserInit(struct JsonParser *parser, struct JsonHandler *h, struct JsonAllocator *a);

// Parse a JSON document
// Input buffer does not need to be null-terminated.
MEWJSON_NODISCARD
enum JsonParseStatus jsonParse(const char *input, JsonSize length, struct JsonParser *parser);

// Return the length of a JSON string
JsonSize jsonLength(const JsonValue *val);

// Return an enumerator describing the type of JSON value
enum JsonType jsonType(const JsonValue *val);

// Return the value of a JSON string
// The returned string is not null-terminated, but may contain embedded null bytes. Call
// jsonLength() on val to get the length of the string in bytes.
const char *jsonString(const JsonValue *val);

// Return the value of a JSON integer
// An integer is a JSON number that doesn't contain any "floating-point indicators" (e, E, or .).
// Such numbers are parsed as 64-bit signed integers. Overflows are handled by parsing the value
// as a real number instead.
int64_t jsonInteger(const JsonValue *val);

// Return the value of a JSON real number
// If the magnitude of the number is too great to be represented as a double-precision float,
// then +/-Inf is returned as appropriate. Note that jsonWrite() will convert such numbers to
// null.
double jsonReal(const JsonValue *val);

// Return the value of a JSON boolean
JsonBool jsonBoolean(const JsonValue *val);

#endif // MEWJSON_H
