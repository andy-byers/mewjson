// mewjson: a library for (de)serializing JSON text
// Distributed under the MIT License (http://opensource.org/licenses/MIT)
//
// This file contains the API documentation for mewjson. There are interfaces for reading and
// writing JSON (jsonRead, jsonWrite), as well as accessing an in-memory representation of the
// data (JsonDocument, JsonValue, etc.).
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

// Parsed JSON document
typedef struct JsonDocument JsonDocument;

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

// JSON parser context
struct JsonParser {
    // Byte offset that the parser stopped on in the input buffer.
    JsonSize offset;

    // Status code pertaining to the last parse. Values is kParseOk on success, and some other
    // variant on failure.
    enum JsonParseStatus status;

    // Private members.
    struct JsonParserPrivate {
        JsonDocument *doc;
        JsonSize upIndex;
        JsonSize depth;
    } x;
};

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
JsonDocument *jsonRead(const char *input, JsonSize length, struct JsonParser *parser);

// JSON value (object, array, string, integer, real number, boolean, or null)
typedef struct JsonValue JsonValue;

// Return the root value of a document
// A successfully-parsed document will always have a single root value. The root can be of
// any type.
JsonValue *jsonRoot(JsonDocument *doc);

// Write a JSON value and all its descendents to a buffer
// Returns the number of bytes needed to hold the serialized JSON data. jsonWrite() will write as
// much as possible if there is not enough space. Call jsonWrite(NULL, 0, root) to check how many
// bytes are required to write a particular value. The written text is always a valid JSON document.
JsonSize jsonWrite(char *buffer, JsonSize length, JsonValue *root);

enum JsonQueryStatus {
    kQueryOk,
    kQueryNoMemory,
    kQueryNotFound,
    kQueryPathInvalid,
    kQueryStatusCount
};

// Find a descendent of the given root value using a JSON pointer
MEWJSON_NODISCARD
enum JsonQueryStatus jsonFind(JsonValue *root, const char *jsonPtr, JsonSize ptrSize,
                              JsonValue **out);

// Add an object member key to a JSON pointer
JsonSize jsonPointerWriteKey(char *jsonPtr, JsonSize ptrSize, const char *key, JsonSize length);

// Add an array index to a JSON pointer
JsonSize jsonPointerWriteIndex(char *jsonPtr, JsonSize ptrSize, JsonSize index);

// Write the JSON pointer that refers to the given JSON value, relative to the root
// Returns the number of bytes needed to hold the JSON pointer, or -1 if the search value is not a
// descendent of the root value.
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

// Find the index of the object member with the given key
// Returns -1 if the member does not exist.
JsonSize jsonObjectFind(JsonValue *obj, const char *key, JsonSize length);

// Return a child value from a JSON object or array
// Returns NULL if the index is out of bounds.
JsonValue *jsonContainerGet(JsonValue *container, JsonSize index);

// Determines the behavior of a cursor during traversal
// The following examples show how each mode behaves. The numbers indicate the order in
// which the various JSON values are encountered. Each example assumes that the cursor is
// started on the root value: an array in the first case and an integer in the second.
//  Document  | kCursorNormal | kCursorRecursive
// -----------|---------------|------------------
//   [        |               | 1
//     "a",   | 1             | 2
//     [      | 2             | 3
//       true |               | 4
//     ]      |               |
//   ]        |               |
//
//  Document  | kCursorNormal | kCursorRecursive
// -----------|---------------|------------------
//   42       |               | 1

enum JsonCursorMode {
    kCursorNormal,
    kCursorRecursive,
};

struct JsonCursor {
    JsonValue *root;
    JsonValue *value;
    enum JsonCursorMode mode;
};

// Initialize a cursor for traversal over a JSON value
void jsonCursorInit(JsonValue *root, enum JsonCursorMode mode, struct JsonCursor *c);

// Return true if a cursor is valid, false otherwise
JsonBool jsonCursorIsValid(const struct JsonCursor *c);

// Move a cursor to the next JSON value
void jsonCursorNext(struct JsonCursor *c);

// Return a pointer to the first byte of the current object member's key, or NULL if the cursor
// is not positioned on an object member
// If the length parameter is not NULL, this function will write to it the length of the key in
// bytes.
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
