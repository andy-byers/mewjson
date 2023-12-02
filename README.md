> NOTE: This library is under active development.
> Please don't use it for anything important!

# mewjson
a tiny JSON parser

## Requirements
The library requires the following to compile:
+ CMake version 3.14 or later
+ C compiler with support for C99

## Caveats
+ Only supports SAX-style (callback-based) parsing
+ Parser uses dynamic memory, usually about equal to the size of the input
+ Real number parsing is very slow (uses `strtod()`)

## API
mewjson aims to provide a small, convenient API for parsing and validating JSON data.

### Initialize a parser
The second parameter to `jsonParserInit()` is a pointer to `struct JsonHandler`, and the third a pointer to `struct JsonAllocator`.
If a `struct JsonHandler` is provided, its callback members are called as the document is parsed.
Otherwise, `jsonParse` will do nothing but validate the document.
If the allocator parameter is nonnull, the provided allocation functions are used to get memory for the parser. 
Otherwise, the standard C allocation routines `malloc`, `realloc`, and `free` are used.
```C
struct JsonParser parser;
struct JsonAllocator a = {/* user-defined allocation functions go here */};
struct JsonHandler h = {/* user-defined callbacks go here */};
jsonParserInit(&parser, &h, &a); // Must be called prior to jsonParse()
```

### Parse a JSON document
The callbacks in `h` are called as values and structural elements are encountered during the parse.
The input buffer does not need to be null-terminated.
```C
static const char kJson[] = "[1,[2,[3],4],5]";
enum JsonStatus s = jsonParse(kJson, (JsonSize)strlen(kJson), &parser);
if (s != kStatusOk) {
    // The s variable indicates what went wrong, and parser.offset indicates where in the
    // input buffer the parser stopped.
    fprintf(stderr, "parser error %d at offset %ld\n",
            s, parser.offset);
    abort();
}
```
