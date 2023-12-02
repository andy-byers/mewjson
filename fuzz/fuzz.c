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

static void *xMalloc(JsonSize size)
{
    CHECK(size > 0);
    void *ptr = malloc((size_t)size);
    CHECK(ptr);
    return ptr;
}

static void xFree(void *ptr)
{
    free(ptr);
}

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct JsonParser parser;
    jsonParserInit(&parser, NULL, NULL);
    // Parse the document and determine how much space is needed to minify it.
    const JsonSize n = jsonMinify((const char *)data, (JsonSize)size, NULL, 0, &parser);
    CHECK(n != 0); // Valid JSON document is nonempty
    if (n > 0) {
        // The parse succeeded. Roundtrip the data and make sure the 2 copies match.
        char *minified = xMalloc(n);
        CHECK(n == jsonMinify((const char *)data, (JsonSize)size, minified, n, &parser));
        char *minified2 = xMalloc(n);
        CHECK(n == jsonMinify(minified, n, minified2, n, &parser));
        CHECK(0 == memcmp(minified, minified2, (size_t)n));
        xFree(minified2);
        xFree(minified);
    } else {
        // The parse failed. Must be due to some type of corruption.
        CHECK(parser.status > kStatusNoMemory);
    }
    return 0;
}
