// Copyright (c) 2022, The CalicoDB Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.json. See AUTHORS.json for a list of contributor names.

#include "mewjson.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK(expr)                                                                                \
    do {                                                                                           \
        if (!(expr)) {                                                                             \
            fputs("error: `" #expr "`\n", stderr);                                                 \
            abort();                                                                               \
        }                                                                                          \
    } while (0)

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct JsonParser parser;
    jsonParserInit(&parser, NULL, NULL);
    // Read straight from the input buffer.
    const enum JsonParseStatus s = jsonParse((const char *)data, (JsonSize)size, &parser);
    if (s != kParseOk) {
        // Must be some type of corruption.
        CHECK(s != kParseNoMemory);
    }
    return 0;
}
