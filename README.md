> NOTE: This library is under active development.
> Please don't use it for anything important!

# mewjson
a tiny JSON library

## Requirements
The library requires the following to compile:
+ CMake version 3.14 or later
+ C compiler with support for C11

## Features
+ Stores whole numbers between -9223372036854775808 and 9223372036854775807, inclusive, as 64-bit signed integers
+ Uses a packed parse tree representation to save memory

## Caveats
+ Only supports DOM-style (parse tree-based) parsing
+ Parser uses dynamic memory
+ Real number handling is slow and breaks in certain locales (uses `strtod()` and `snprintf()`)
+ Parse tree is immutable

## API
mewjson aims to provide a small, convenient API for parsing and validating JSON data.

### Initialize a parser
The second parameter to `jsonParserInit()` is a pointer to `struct JsonAllocator`.
If nonnull, the provided allocation functions are used to get memory for the parser. 
Otherwise, the standard C allocation routines `malloc`, `realloc`, and `free` are used.
```C
struct JsonParser parser;
struct JsonAllocator a = {/* user-defined allocation functions go here */};
jsonParserInit(&parser, &a); // Must be called prior to jsonParse()
```

### Parse a JSON document
The callbacks in `h` are called as values and structural elements are encountered during the parse.
The input buffer does not need to be null-terminated.
```C
static const char kJson[] = "[1,[2,[3],4],5]";
JsonDocument *doc = jsonParse(kJson, (JsonSize)strlen(kJson), &parser);
if (!doc) {
    // parser.status indicates what went wrong, and parser.offset indicates where in the input
    // buffer the parser stopped.
    fprintf(stderr, "parser error %d at offset %ld\n",
            parser.status, parser.offset);
    abort();
}
```

### Cleanup
```C
jsonDestroyDocument(doc);
```

## TODO
+ Document the rest of the current API (jsonWrite(), jsonCloneDocument())
+ Prefix the document encoding with a checksum over all of its bytes
+ Allow direct access to the binary encoding (it can be saved and loaded, using the checksum for validation)
+ Consider supporting JSON patch and/or JSON merge patch
  + Would have to be an API that takes 2 documents and returns another one, since the parse trees aren't easy to modify
  + Makes `jsonWrite()` much more useful
## Credits
+ The awesome folks on [this r/C_Programming thread](https://www.reddit.com/r/C_Programming/comments/18aqehf/json_parser_project/) (posted around 6983ae8), who were kind enough to review this project
+ Variable-length integer encoding/decoding functions were modified from LevelDB
+ Logic for decoding integers is from SQLite's JSON module
+ Idea to use a packed representation is from SQLite's JSONB module
+ State transition lookup table was inspired by rapidjson
+ Signed varint encoding is from the Go repo
