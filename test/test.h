#ifndef MEWJSON_TEST_H
#define MEWJSON_TEST_H

#include "mewjson.h"
#include <stdio.h>
#include <stdlib.h>

#define CHECK(expr)                                                                                \
    do {                                                                                           \
        if (!(expr)) {                                                                             \
            fprintf(stderr, "error in %s on line %d: expected `" #expr "`\n", __func__, __LINE__); \
            abort();                                                                               \
        }                                                                                          \
    } while (0)

#define SIZEOF(x) (JsonSize)sizeof(x)
#define COUNTOF(a) (SIZEOF(a) / SIZEOF((a)[0]))

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define CAT(x, y) XSTR(x##y)

void checkForLeaks();
void *leakCheckMalloc(size_t size);
void *leakCheckRealloc(void *ptr, size_t size);
void leakCheckFree(void *ptr);

JsonBool areRealsEqual(double x, double y);

#endif // MEWJSON_TEST_H