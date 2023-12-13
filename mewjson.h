// mewjson: a tiny JSON library
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

typedef struct JsonDocument JsonDocument;

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

// General-purpose allocation routines used by the parser
// User-provided allocators must return memory that is aligned to at least _Alignof(max_align_t).
struct JsonAllocator {
    void *ctx;
    void *(*malloc)(void *ctx, size_t size);
    void *(*realloc)(void *ctx, void *ptr, size_t oldSize, size_t newSize);
    void (*free)(void *ctx, void *ptr, size_t size);
};

// JSON parser context
struct JsonParser {
    struct JsonAllocator a;

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
void jsonParserInit(struct JsonParser *parser, struct JsonAllocator *a);

// Parse a JSON document
// Input buffer does not need to be null-terminated.
MEWJSON_NODISCARD
JsonDocument *jsonParse(const char *input, JsonSize length, struct JsonParser *parser);

// Clone a document
// jsonDestroyDocument() must be called on both documents when they are no longer needed.
MEWJSON_NODISCARD
JsonDocument *jsonCloneDocument(JsonDocument *doc);

// Free memory associated with a document
void jsonDestroyDocument(JsonDocument *doc);

// Return the root value of a document
// A successfully-parsed document will always have a single root value. The root can be of
// any type.
JsonValue *jsonRoot(JsonDocument *doc);

// Write a JSON value and all its descendents to a buffer
// Returns the number of bytes needed to hold the serialized JSON data. jsonStringify() will write
// as much as possible if there is not enough space. Call jsonStringify(NULL, 0, root) to check how
// many bytes are required to write a particular value. The written text is always a valid JSON
// document.
JsonSize jsonStringify(char *buffer, JsonSize length, JsonValue *root);

// Return an enumerator describing the type of JSON value
enum JsonType jsonType(const JsonValue *value);

// Return the value of a JSON string
// The returned string is not null-terminated, but may contain embedded null bytes. Pass the
// address of a JsonSize to get the length of the string in bytes.
const char *jsonString(const JsonValue *value, JsonSize *length);

// Return the value of a JSON integer
// An integer is a JSON number that doesn't contain any "floating-point indicators" (e, E, or .).
// Such numbers are parsed as 64-bit signed integers. Overflows are handled by parsing the value
// as a real number instead.
int64_t jsonInteger(const JsonValue *value);

// Return the value of a JSON real number
// If the magnitude of the number is too great to be represented as a double-precision float,
// then +/-Inf is returned as appropriate. Note that jsonStringify() will convert such numbers to
// null.
double jsonReal(const JsonValue *value);

// Return the value of a JSON boolean
JsonBool jsonBoolean(const JsonValue *value);

// Find the index of the object member with the given key
// Returns -1 if the member does not exist.
JsonSize jsonObjectFind(JsonValue *obj, const char *key, JsonSize length);

// Return a child value from a JSON object or array
// Returns NULL if the index is out of bounds.
JsonValue *jsonContainerGet(JsonValue *container, JsonSize index);

// Return the number of children in a JSON container
JsonSize jsonContainerLength(const JsonValue *value);

// Determines the behavior of a cursor during traversal
// kCursorNormal visits the immediate children of a container node, and kCursorRecursive
// visits all nodes rooted at the target node, including the target node itself.
// The following examples show how each mode behaves. The numbers indicate the order in
// which the various JSON values are encountered. Each example assumes that the cursor is
// started on the root value: an array in the first case and an integer in the second.
//  Document  | kCursorNormal | kCursorRecursive | Node
// -----------|---------------|------------------|--------------
//   [        |               | 1                | [...]
//     "a",   | 1             | 2                | "a"
//     [      | 2             | 3                | [...]
//       true |               | 4                | true
//     ]      |               |                  |
//   ]        |               |                  |
//
//  Document  | kCursorNormal | kCursorRecursive | Node
// -----------|---------------|------------------|--------------
//   42       |               | 1                | 42
enum JsonCursorMode {
    kCursorNormal,
    kCursorRecursive,
};

struct JsonCursor {
    char *root;
    char *cursor;
    char *parent;
    JsonSize total;
    enum JsonCursorMode mode;
};

JsonValue *jsonCursorValue(const struct JsonCursor *c);

JsonValue *jsonCursorParent(const struct JsonCursor *c);

// Initialize a cursor for traversal over some portion of a JSON document
// Must be called before any other jsonCursor*() function.
void jsonCursorInit(JsonValue *root, enum JsonCursorMode mode, struct JsonCursor *c);

// Return true if a cursor is valid, false otherwise
JsonBool jsonCursorIsValid(const struct JsonCursor *c);

// Move a cursor to the next JSON value node
void jsonCursorNext(struct JsonCursor *c);

// Return a pointer to a serialized parse tree representing a JSON value
// Writes the number of bytes occupied by the parse tree to the "length" parameter.
JsonSize jsonSave(char *buffer, JsonSize length, JsonValue *value);

// Return a document built from a serialized parse tree
// The data (and length) passed to this function must be exactly what was returned by a prior
// call to jsonSave(). Otherwise, the behavior is undefined.
MEWJSON_NODISCARD
JsonDocument *jsonLoad(const char *binary, JsonSize length, struct JsonAllocator *a);

#endif // MEWJSON_H
