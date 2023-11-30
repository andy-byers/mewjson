> NOTE: This library is under active development.
> Please don't use it for anything important!

# mewjson
a library for (de)serializing JSON text

mewjson aims to provide a small, convenient API for reading and writing JSON data.
The library provides interfaces for parsing JSON text into an in-memory tree representation, as well as and serializing a parse tree back to text.
While loaded in-memory, a JSON document can be iterated over with a cursor.

## Requirements
The library requires the following to compile:
+ CMake version 3.14 or later
+ C compiler with support for C99

## API
Initialize a JSON parser object.
The second parameter to `jsonParserInit()` is a pointer to `struct JsonAllocator`.
If nonnull, the provided allocation functions are used to get memory for the parser. 
Otherwise, the standard C allocation routines `malloc`, `realloc`, and `free` are used.
```C
struct JsonParser parser;
jsonParserInit(&parser, NULL);
```

Parse a JSON document into a `JsonDocument` object.
The input buffer does not need to be null-terminated.
```C
static const char kJson[] = "[1,[2,[3],4],5]";
JsonDocument *doc = jsonRead(kJson, (JsonSize)strlen(kJson), &parser);
if (!doc) {
    // parser.status indicates what went wrong, and parser.offset indicates where in the
    // input buffer the parser stopped.
    fprintf(stderr, "parser error %d at offset %ld\n",
            parser.status, parser.offset);
    abort();
}
// Get a pointer to the root JSON value: an array with 3 elements.
JsonValue *root = jsonRoot(doc);
assert(jsonType(root) == kTypeArray);
assert(jsonLength(root) == 3);
```

Use a recursive JSON cursor to print all integers in the document.
```C
struct JsonCursor c;
jsonCursorInit(jsonRoot(doc), kCursorRecursive, &c);
while (jsonCursorIsValid(&c)) {
    if (jsonType(c.value) == kTypeInteger) {
        printf("found integer %" PRId64 "\n", jsonInteger(c.value));
    }
    jsonCursorNext(&c);
}
```

Write just the middle array, `[2,[3],4]`, to a buffer.
```C
// Get the target array from the root array.
JsonValue *arr = jsonContainerGet(root, 1);

// Determine exactly how many bytes are needed to hold the JSON text, and allocate a buffer with
// space for a null-terminator.
const JsonSize n = jsonWrite(NULL, 0, arr);
char *buffer = malloc(n + 1);

// Write the array to the buffer.
jsonWrite(buffer, n, arr);
buffer[n] = '\0';

// Print the array and clean up.
puts(buffer);
free(buffer);
```

Release memory allocated for the document object.
```C
jsonDestroyDocument(doc);
```