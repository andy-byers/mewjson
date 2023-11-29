// mewjson: a library for (de)serializing JSON text
// Distributed under the MIT License (http://opensource.org/licenses/MIT)
//
// This file contains the API documentation for mewjson. There are interfaces for reading and
// writing JSON (jsonParse, jsonBuild, jsonGenerate, etc.), as well as accessing an in-memory
// representation of the data (JsonDocument, JsonValue, etc.).
//
// General API rules:
// 1. Pointers passed to API functions must be valid, unless otherwise noted
// 2. Indices passed to API functions must be in bounds, unless otherwise noted
// 3. Functions that expect a JsonValue of a specific type, like jsonArrayGet(), jsonObjectGet(),
//    etc., will break if passed the wrong type (make sure to check the type first by calling
//    jsonType() on the JsonValue in question)
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

// JSON parsing context
typedef struct JsonParser JsonParser;

// Return a new JSON parser
// jsonDestroyParser() must be called on the returned parser when it is no longer needed.
MEWJSON_NODISCARD
JsonParser *jsonCreateParser(void);

// Free memory associated with a parser
void jsonDestroyParser(JsonParser *parser);

// Status code produced by the parser
enum JsonParseStatus {
    kParseOk,
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

// Return the last error encountered by the parser
enum JsonParseStatus jsonLastStatus(const JsonParser *parser);

// Return the offset of the last parsed byte in the input stream
// If an error occurred during the last parse, this function shows roughly where it happened.
JsonSize jsonLastOffset(const JsonParser *parser);

// Parsed JSON document
typedef struct JsonDocument JsonDocument;

// Free memory associated with a document
void jsonDestroyDocument(JsonDocument *doc);

// Build an in-memory representation of a JSON document
// Returns a pointer to JsonDocument representing the parsed document on success, and NULL on
// failure. The reason for failure can be queried by calling jsonGetError() on the parser. The
// input buffer does not need to be null-terminated. It must remain valid for as long as the
// returned JsonDocument is alive, however (the document stores pointers into the buffer for
// strings without any escaped characters). jsonDestroyDocument() must be called on the returned
// document when it is no longer needed.
MEWJSON_NODISCARD
JsonDocument *jsonRead(const char *input, JsonSize length, JsonParser *parser);

// JSON value (object, array, string, integer, real number, boolean, or null)
typedef struct JsonValue JsonValue;

// Return the root value of a document
// A successfully-parsed document will always have a single root value. The root can be of
// any type.
JsonValue *jsonRoot(JsonDocument *doc);

// Write a JSON value and all its descendents to a buffer
// Returns the number of bytes needed to hold the serialized JSON data. jsonWrite() will write as
// much as possible if there is not enough space. Call jsonWrite(NULL, 0, root) to check how many
// bytes are required to write a particular value. This function allocates memory in order to
// traverse the document and will return -1 if an allocation fails. The written text is always a
// valid JSON document.
MEWJSON_NODISCARD
JsonSize jsonWrite(char *buffer, JsonSize length, JsonValue *root);

enum JsonQueryStatus {
    kQueryOk,
    kQueryNoMemory,
    kQueryNotFound,
    kQueryPathInvalid,
    kQueryStatusCount
};

MEWJSON_NODISCARD
enum JsonQueryStatus jsonFind(JsonValue *root, const char *jsonPtr, JsonSize ptrSize,
                              JsonValue **out);

JsonSize jsonPointerWriteKey(char *jsonPtr, JsonSize ptrSize, const char *key, JsonSize length);

JsonSize jsonPointerWriteIndex(char *jsonPtr, JsonSize ptrSize, JsonSize index);

JsonSize jsonLocate(JsonValue *root, char *jsonPtr, JsonSize ptrSize, JsonValue *val);

// Return the length of a JSON value
// When called on an object or array, this function returns the number of elements or members,
// respectively. For strings, this function returns the number of bytes. For all other types,
// this function returns 0.
JsonSize jsonLength(const JsonValue *val);

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

// Find the index of the member with the given key
// Returns -1 if the member does not exist.
JsonSize jsonObjectFind(JsonValue *obj, const char *key, JsonSize length);

// Return a child from a JSON object or array
// Returns NULL if the index is out of bounds.
JsonValue *jsonContainerGet(JsonValue *container, JsonSize index);

enum JsonCursorMode {
    kCursorNormal,
    kCursorRecursive,
};

struct JsonCursor {
    JsonValue *root;
    JsonValue *value;
    enum JsonCursorMode mode;
};

void jsonCursorInit(JsonValue *root, enum JsonCursorMode mode, struct JsonCursor *c);

JsonBool jsonCursorIsValid(const struct JsonCursor *c);

void jsonCursorNext(struct JsonCursor *c);

const char *jsonCursorKey(struct JsonCursor *c, JsonSize *length);

// General-purpose allocation routines used by the library
struct JsonAllocator {
    void *(*malloc)(size_t);
    void *(*realloc)(void *, size_t);
    void (*free)(void *);
};

// Set the general-purpose allocator used to get memory for the library
// This function is not threadsafe.
void jsonSetAllocator(struct JsonAllocator a);

#endif // MEWJSON_H
