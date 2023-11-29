> NOTE: This library is under active development.
> Please don't use it for anything important!

# mewjson
a library for (de)serializing JSON text

mewjson aims to provide a small, convenient API for reading and writing JSON data.
The library provides interfaces for parsing JSON text into an in-memory document representation, as well as and serializing a parsed document back to text.
While loaded in-memory, a JSON document can be queried using JSON pointers, or iterated over with a cursor.

## Requirements
The library requires the following to compile:
+ CMake version 3.14 or later
+ C compiler with support for C99