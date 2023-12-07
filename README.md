> NOTE: This library is under active development.
> Please don't use it for anything important!

# mewjson
a tiny JSON library

## Requirements
The library requires the following to compile:
+ CMake version 3.14 or later
+ C compiler with support for C11

## Features
+ Uses a packed parse tree representation to save memory

## Caveats
+ Only supports DOM-style (parse tree-based) parsing
+ Parser uses dynamic memory
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

## Credits
+ The awesome folks on [this r/C_Programming thread](https://www.reddit.com/r/C_Programming/comments/18aqehf/json_parser_project/), who were kind enough to review this project
+ Variable-length integer encoding/decoding functions were modified from LevelDB
+ Logic for decoding integers is from SQLite's JSON module
+ Idea to use a packed representation is from SQLite's JSONB module
+ State transition lookup table was inspired by rapidjson
+ Signed varint encoding is from the Go repo
