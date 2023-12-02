// mewjson: a tiny JSON parser
// Distributed under the MIT License (http://opensource.org/licenses/MIT)

#ifndef MEWJSON_H
#define MEWJSON_H

#include <stddef.h>
#include <stdint.h>

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

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
    kTypeCount //
};

// Status code produced by the parser
enum JsonStatus {
    kStatusOk,
    kStatusStopped,
    kStatusNoMemory,
    kStatusExceededMaxDepth,
    kStatusStringInvalidEscape,
    kStatusStringInvalidCodepoint,
    kStatusStringMissingHighSurrogate,
    kStatusStringMissingLowSurrogate,
    kStatusStringUnescapedControl,
    kStatusStringInvalidUtf8,
    kStatusLiteralInvalid,
    kStatusNumberMissingExponent,
    kStatusNumberMissingFraction,
    kStatusNumberInvalid,
    kStatusContainerNotClosed,
    kStatusSyntaxError,
    kStatusCount
};

// User-defined callbacks to call as the document is parsed
struct JsonHandler {
    void *ctx;
    JsonBool (*acceptKey)(void *ctx, const char *str, JsonSize len);
    JsonBool (*acceptValue)(void *ctx, const JsonValue *value);
    JsonBool (*beginContainer)(void *ctx, JsonBool isObject);
    JsonBool (*endContainer)(void *ctx, JsonBool isObject);
};

// Return the length of a JSON string
// Returns 0 if the given value is not a string.
JsonSize jsonLength(const JsonValue *value);

// Return an enumerator describing the type of JSON value
enum JsonType jsonType(const JsonValue *value);

// Return the value of an unescaped JSON string
// The returned string is not null-terminated, but may contain embedded null bytes. Call
// jsonLength(value) to get the length of the string in bytes.
const char *jsonString(const JsonValue *value);

// Return the value of a JSON integer
// An integer is a JSON number that doesn't contain any "floating-point indicators" (e, E, or .).
// Such numbers are parsed as 64-bit signed integers. Overflows are handled by parsing the value
// as a real number instead.
int64_t jsonInteger(const JsonValue *value);

// Return the value of a JSON real number
// If the magnitude of the number is too great to be represented as a double-precision float,
// then +/-Inf is returned as appropriate. Note that jsonWrite() will convert such numbers to
// null.
double jsonReal(const JsonValue *value);

// Return the value of a JSON boolean
JsonBool jsonBoolean(const JsonValue *value);

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

    enum JsonStatus status;
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
enum JsonStatus jsonParse(const char *input, JsonSize length, struct JsonParser *parser);

// Minify a JSON document
// If a handler was set on the parser, it will be ignored for the duration of this function.
// Returns the number of bytes needed to hold the minified text. If there was not enough space
// in the output buffer, then the partially-written contents may be malformed (in addition to
// being truncated). This function does not add a null terminator. On failure, returns -1 and
// sets parser->status and parser->offset to indicate what went wrong and where the parser
// stopped, respectively.
MEWJSON_NODISCARD
JsonSize jsonMinify(const char *input, JsonSize inputLen, char *output, JsonSize outputLen,
                    struct JsonParser *parser);

#endif // MEWJSON_H
