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

static char *intoText(JsonDocument *doc, JsonSize *writeLen)
{
    CHECK(doc);
    JsonValue *root = jsonRoot(doc);
    if (*writeLen == 0) {
        *writeLen = jsonStringify(NULL, 0, root);
    }
    const JsonSize n = *writeLen;
    CHECK(n > 0);

    char *result = malloc((size_t)n);
    CHECK(result);

    CHECK(n == jsonStringify(result, n, root));
    jsonDestroyDocument(doc);
    return result;
}

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct JsonParser parser;
    jsonParserInit(&parser, NULL);
    JsonDocument *doc = jsonParse((const char *)data, (JsonSize)size, &parser);
    if (doc) {
        CHECK(parser.status == kStatusOk);
        CHECK(parser.offset > 0); // Valid JSON document is nonempty
        // Roundtrip and compare the text.
        JsonSize n = 0;
        char *result = intoText(doc, &n);
        JsonDocument *doc2 = jsonParse(result, n, &parser);
        char *result2 = intoText(doc2, &n);
        CHECK(0 == memcmp(result, result2, (size_t)n));
        free(result2);
        free(result);
    } else {
        // The parse failed. Must be due to corruption.
        CHECK(parser.status > kStatusNoMemory);
    }
    return 0;
}
