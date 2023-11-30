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

static void *xMalloc(size_t size)
{
    void *ptr = malloc(size);
    CHECK(ptr);
    return ptr;
}

static void xFree(void *ptr)
{
    free(ptr);
}

static char *writeJsonText(JsonDocument *doc, JsonSize *textLen)
{
    if (*textLen == 0) {
        // Determine how many bytes are required to hold the JSON text.
        *textLen = jsonWrite(NULL, 0, jsonRoot(doc));
    }
    const JsonSize len = *textLen;
    CHECK(len > 0); // Valid JSON cannot be empty.
    char *text = xMalloc(len + 1);
    CHECK(len == jsonWrite(text, len + 1, jsonRoot(doc)));
    return text;
}

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct JsonParser parser;
    jsonParserInit(&parser, NULL);
    // Read straight from the input buffer.
    JsonDocument *doc = jsonRead((const char *)data, (JsonSize)size, &parser);
    if (doc) {
        CHECK(parser.status == kParseOk);
        // Serialize to a different buffer.
        JsonSize textLen = 0;
        char *copy1 = writeJsonText(doc, &textLen);
        jsonDestroyDocument(doc);

        // Read the text we just wrote.
        JsonDocument *doc2 = jsonRead(copy1, textLen, &parser);
        CHECK(parser.status == kParseOk);
        CHECK(doc2);

        // Write again to another buffer.
        char *copy2 = writeJsonText(doc2, &textLen);
        jsonDestroyDocument(doc2);

        // Make sure the 2 copies we wrote are exactly the same.
        CHECK(0 == memcmp(copy1, copy2, textLen));
        xFree(copy1);
        xFree(copy2);
    } else {
        // Must be some type of corruption.
        CHECK(parser.status != kParseNoMemory);
    }
    return 0;
}
