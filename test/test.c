#include "test.h"
#include <float.h>
#include <math.h>

static JsonSize sBytesUsed = 0;

void *leakCheckMalloc(size_t size)
{
    JsonSize *ptr = malloc(sizeof(JsonSize) + size);
    if (ptr) {
        sBytesUsed += (JsonSize)size;
        *ptr++ = (JsonSize)size;
    }
    return ptr;
}

void *leakCheckRealloc(void *ptr, size_t size)
{
    JsonSize oldSize = 0;
    JsonSize *hdr = ptr;
    if (hdr) {
        --hdr;
        oldSize = hdr[0];
    }
    JsonSize *newPtr = realloc(hdr, sizeof(JsonSize) + size);
    if (newPtr) {
        sBytesUsed += (JsonSize)size - oldSize;
        *newPtr++ = (JsonSize)size;
    }
    return newPtr;
}

void leakCheckFree(void *ptr)
{
    if (ptr) {
        JsonSize *hdr = ptr;
        --hdr;
        sBytesUsed -= hdr[0];
        CHECK(sBytesUsed >= 0);
        free(hdr);
    }
}

void checkForLeaks()
{
    CHECK(sBytesUsed == 0);
}

// Modified from P-Gn's answer to https://stackoverflow.com/questions/4915462.
inline JsonBool areRealsEqual(double x, double y)
{
    static const double kTheta = DBL_MIN;
    static const double kEpsilon = 128 * DBL_EPSILON;
    if (x == y) {
        return 1;
    }
    const double diff = fabs(x - y);
    const double norm = fmin(fabs(x + y), DBL_MAX);
    return diff < fmax(kTheta, kEpsilon * norm);
}
