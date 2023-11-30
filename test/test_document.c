#include "test.h"
#include <inttypes.h>
#include <string.h>

static int sOomCounter = -1;

static void *testMalloc(size_t size)
{
    if (sOomCounter == 0) {
        return NULL;
    } else if (sOomCounter > 0) {
        --sOomCounter;
    }
    return leakCheckMalloc(size);
}

static void *testRealloc(void *ptr, size_t size)
{
    if (sOomCounter == 0) {
        return NULL;
    } else if (sOomCounter > 0) {
        --sOomCounter;
    }
    return leakCheckRealloc(ptr, size);
}

static void testFree(void *ptr)
{
    leakCheckFree(ptr);
}

static void testSetAllocator(struct JsonParser *parser)
{
    jsonParserInit(parser, &(struct JsonAllocator){
                               testMalloc,
                               testRealloc,
                               testFree,
                           });
}

// Scratch memory for unescaping JSON pointers
struct StringBuilder {
    char *ptr;
    JsonSize len;
    JsonSize cap;
};

static void sbEnsureSpace(struct StringBuilder *sb, JsonSize addedLen)
{
    const JsonSize len = sb->len + addedLen;
    if (len > sb->cap) {
        JsonSize cap = 4;
        while (cap < len) {
            cap *= 2;
        }
        char *ptr = realloc(sb->ptr, (size_t)cap);
        CHECK(ptr);
        sb->ptr = ptr;
        sb->cap = cap;
    }
}

static void sbAppendChar(struct StringBuilder *sb, char c)
{
    sbEnsureSpace(sb, 1);
    sb->ptr[sb->len] = c;
    ++sb->len;
}

static void sbAppendSlice(struct StringBuilder *sb, const char *ptr, JsonSize len)
{
    sbEnsureSpace(sb, len);
    memcpy(sb->ptr + sb->len, ptr, len);
    sb->len += len;
}

static void sbAppendString(struct StringBuilder *sb, const char *str)
{
    const JsonSize length = (JsonSize)strlen(str);
    sbAppendSlice(sb, str, length);
}

static const char *sbFinish(struct StringBuilder *sb)
{
    sbAppendChar(sb, '\0');
    sb->len = 0;
    return sb->ptr;
}

static void testcaseInit(const char *name)
{
    puts(name);
    sOomCounter = -1;
}

static void testcaseLongStrings(void)
{
    testcaseInit("long_strings");

    static const JsonSize kQuarterGiB = 1 << 28;
    char *buffer = malloc(kQuarterGiB * 2 + 1024);
    static const char kInitialContents[] = "[\"*\",\"***\",\"*****\"";
    strcpy(buffer, kInitialContents);
    char *ptr = buffer + strlen(kInitialContents);
    for (JsonSize len = kQuarterGiB / 2; len <= kQuarterGiB; len *= 2) {
        strcpy(ptr, ",\"");
        ptr += 2;
        memset(ptr, '*', len);
        ptr += len;
        *ptr++ = '"';
    }
    strcpy(ptr, "]");

    struct JsonParser parser;
    testSetAllocator(&parser);
    JsonDocument *doc = jsonRead(buffer, (JsonSize)strlen(buffer), &parser);
    CHECK(parser.status == kParseOk);
    CHECK(doc);
    jsonDestroyDocument(doc);
    free(buffer);
}

// Modified from https://json.org/example.html. Extra nested containers added in a
// few places to cause more node allocations.
static const char kOomExample[] =
    "{\"web-app\": {\n"
    "  \"servlet\": [   \n"
    "    {\n"
    "      \"servlet-name\": \"cofaxCDS\",\n"
    "      \"servlet-class\": \"org.cofax.cds.CDSServlet\",\n"
    "      \"init-param\": {\n"
    "        \"configGlossary:installationAt\": \"Philadelphia, PA\",\n"
    "        \"configGlossary:adminEmail\": \"ksm@pobox.com\",\n"
    "        \"configGlossary:poweredBy\": \"Cofax\",\n"
    "        \"configGlossary:poweredByIcon\": \"/images/cofax.gif\",\n"
    "        \"configGlossary:staticPath\": \"/content/static\",\n"
    "        \"templateProcessorClass\": \"org.cofax.WysiwygTemplate\",\n"
    "        \"templateLoaderClass\": \"org.cofax.FilesTemplateLoader\",\n"
    "        \"templatePath\": \"templates\",\n"
    "        \"templateOverridePath\": \"\",\n"
    "        \"defaultListTemplate\": \"listTemplate.htm\",\n"
    "        \"defaultFileTemplate\": \"articleTemplate.htm\",\n"
    "        \"useJSP\": false,\n"
    "        \"jspListTemplate\": \"listTemplate.jsp\",\n"
    "        \"jspFileTemplate\": \"articleTemplate.jsp\",\n"
    "        \"cachePackageTagsTrack\": 200,\n"
    "        \"cachePackageTagsStore\": 200,\n"
    "        \"cachePackageTagsRefresh\": 60,\n"
    "        \"cacheTemplatesTrack\": 100,\n"
    "        \"cacheTemplatesStore\": 50,\n"
    "        \"cacheTemplatesRefresh\": 15,\n"
    "        \"cachePagesTrack\": 200,\n"
    "        \"cachePagesStore\": 100,\n"
    "        \"cachePagesRefresh\": 10,\n"
    "        \"cachePagesDirtyRead\": 10,\n"
    "        \"searchEngineListTemplate\": \"forSearchEnginesList.htm\",\n"
    "        \"searchEngineFileTemplate\": \"forSearchEngines.htm\",\n"
    "        \"searchEngineRobotsDb\": \"WEB-INF/robots.db\",\n"
    "        \"useDataStore\": true,\n"
    "        \"dataStoreClass\": \"org.cofax.SqlDataStore\",\n"
    "        \"redirectionClass\": \"org.cofax.SqlRedirection\",\n"
    "        \"dataStoreName\": \"cofax\",\n"
    "        \"dataStoreDriver\": \"com.microsoft.jdbc.sqlserver.SQLServerDriver\",\n"
    "        \"dataStoreUrl\": \"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",\n"
    "        \"dataStoreUser\": \"sa\",\n"
    "        \"dataStorePassword\": \"dataStoreTestQuery\",\n"
    "        \"dataStoreTestQuery\": \"SET NOCOUNT ON;select test='test';\",\n"
    "        \"dataStoreLogFile\": \"/usr/local/tomcat/logs/datastore.log\",\n"
    "        \"dataStoreInitConns\": 10,\n"
    "        \"dataStoreMaxConns\": 100,\n"
    "        \"dataStoreConnUsageLimit\": 100,\n"
    "        \"dataStoreLogLevel\": \"debug\",\n"
    "        \"maxUrlLength\": 500}},\n"
    "    {\n"
    "      \"servlet-name\": \"cofaxEmail\",\n"
    "      \"servlet-class\": \"org.cofax.cds.EmailServlet\",\n"
    "      \"init-param\": {\n"
    "      \"mailHost\": \"mail1\",\n"
    "      \"mailHostOverride\": \"mail2\"}},\n"
    "    {\n"
    "      \"servlet-name\": \"cofaxAdmin\",\n"
    "      \"servlet-class\": \"org.cofax.cds.AdminServlet\"},\n"
    " \n"
    "    {\n"
    "      \"servlet-name\": \"fileServlet\",\n"
    "      \"extraArray\": "
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],"
    "      \"servlet-class\": \"org.cofax.cds.FileServlet\"},\n"
    "    {\n"
    "      \"servlet-name\": \"cofaxTools\",\n"
    "      \"servlet-class\": \"org.cofax.cms.CofaxToolsServlet\",\n"
    "      \"extraArray\": [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4],"
    "      \"init-param\": {\n"
    "        \"templatePath\": \"toolstemplates/\",\n"
    "        \"log\": 1,\n"
    "        \"extraArray1\": [true,false,true,false,true,false,true,false,true,false,true,false],"
    "        \"extraArray2\": [null,null,null,null,null,null,null,null,null,null,null,null,null],"
    "        \"logLocation\": \"/usr/local/tomcat/logs/CofaxTools.log\",\n"
    "        \"logMaxSize\": \"\",\n"
    "        \"dataLog\": 1,\n"
    "        \"dataLogLocation\": \"/usr/local/tomcat/logs/dataLog.log\",\n"
    "        \"dataLogMaxSize\": \"\",\n"
    "        \"removePageCache\": \"/content/admin/remove?cache=pages&id=\",\n"
    "        \"removeTemplateCache\": \"/content/admin/remove?cache=templates&id=\",\n"
    "        \"fileTransferFolder\": \"/usr/local/tomcat/webapps/content/fileTransferFolder\",\n"
    "        \"lookInContext\": 1,\n"
    "        \"adminGroupID\": 4,\n"
    "        \"betaServer\": true}}],\n"
    "  \"servlet-mapping\": {\n"
    "    \"extraArray\": "
    "[1,[2,[3,[4,[5,[6,[7,[8,[9,[0,[1,[2,[3,[4,[5,[6,[7,[8,[9,[0]]]]]]]]]]]]]]]]]]]],\n"
    "    \"cofaxCDS\": \"/\",\n"
    "    \"cofaxEmail\": \"/cofaxutil/aemail/*\",\n"
    "    \"cofaxAdmin\": \"/admin/*\",\n"
    "    \"fileServlet\": \"/static/*\",\n"
    "    \"cofaxTools\": \"/tools/*\"},\n"
    " \n"
    "  \"taglib\": {\n"
    "    \"taglib-uri\": \"cofax.tld\",\n"
    "    \"taglib-location\": \"/WEB-INF/tlds/cofax.tld\"}}}";

static void testcaseReaderOom(void)
{
    testcaseInit("reader_oom");
    struct JsonParser parser;
    testSetAllocator(&parser);

    int failures = 0;
    for (;; ++failures) {
        sOomCounter = failures;
        JsonDocument *doc = jsonRead(kOomExample, (JsonSize)strlen(kOomExample), &parser);
        const enum JsonParseStatus e = parser.status;
        if (e == kParseOk) {
            CHECK(doc);
            jsonDestroyDocument(doc);
            break;
        }
        CHECK(e == kParseNoMemory);
    }
    // Will likely end up failing quite a bit more than 2 times. Just a sanity check.
    CHECK(failures > 2);
}

// Modified from https://json.org/example.html. Added "/menu/" member (empty key).
static const char kQueryExample[] =
    "{\"menu\": {"
    "    \"header\": \"SVG Viewer\","
    "    \"\": [0, {\"a\": 1, \"b\": [2, [3]]}, 4],"
    "    \"items\": ["
    "        {\"id\": \"Open\"},"
    "        {\"id\": \"OpenNew\", \"label\": \"Open New\"},"
    "        null,"
    "        {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},"
    "        {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},"
    "        {\"id\": \"OriginalView\", \"label\": \"Original View\"},"
    "        null,"
    "        {\"id\": \"Quality\"},"
    "        {\"id\": \"Pause\"},"
    "        {\"id\": \"Mute\"},"
    "        null,"
    "        {\"id\": \"Find\", \"label\": \"Find...\"},"
    "        {\"id\": \"FindAgain\", \"label\": \"Find Again\"},"
    "        {\"id\": \"Copy\"},"
    "        {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},"
    "        {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},"
    "        {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},"
    "        {\"id\": \"ViewSource\", \"label\": \"View Source\"},"
    "        {\"id\": \"SaveAs\", \"label\": \"Save As\"},"
    "        null,"
    "        {\"id\": \"Help\"},"
    "        {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}"
    "    ]"
    "}}";

static const char *testRender(JsonValue *root, struct StringBuilder *sb, enum JsonCursorMode mode)
{
    char scratch[32];
    struct JsonCursor c;
    jsonCursorInit(root, mode, &c);
    while (jsonCursorIsValid(&c)) {
        JsonSize keyLen;
        const char *key = jsonCursorKey(&c, &keyLen);
        if (key) {
            sbAppendString(sb, "<k:");
            sbAppendSlice(sb, key, keyLen);
            sbAppendChar(sb, '>');
        }
        switch (jsonType(c.value)) {
            case kTypeString:
                sbAppendString(sb, "<s:");
                sbAppendSlice(sb, jsonString(c.value), jsonLength(c.value));
                sbAppendChar(sb, '>');
                break;
            case kTypeInteger:
                snprintf(scratch, SIZEOF(scratch), "%" PRId64, jsonInteger(c.value));
                sbAppendString(sb, "<i:");
                sbAppendString(sb, scratch);
                sbAppendChar(sb, '>');
                break;
            case kTypeReal:
                snprintf(scratch, SIZEOF(scratch), "%g", jsonReal(c.value));
                sbAppendString(sb, "<r:");
                sbAppendString(sb, scratch);
                sbAppendChar(sb, '>');
                break;
            case kTypeBoolean:
                sbAppendChar(sb, '<');
                sbAppendString(sb, jsonBoolean(c.value) ? "true" : "false");
                sbAppendChar(sb, '>');
                break;
            case kTypeNull:
                sbAppendString(sb, "<null>");
                break;
            default:
                snprintf(scratch, SIZEOF(scratch), "%ld", jsonLength(c.value));
                sbAppendChar(sb, '<');
                sbAppendChar(sb, "ao"[jsonType(c.value) == kTypeObject]);
                sbAppendChar(sb, ':');
                sbAppendString(sb, scratch);
                sbAppendChar(sb, '>');
                break;
        }
        jsonCursorNext(&c);
    }
    return sbFinish(sb);
}

static void testCheckIteration(JsonValue *root, const char *result, enum JsonCursorMode mode)
{
    struct StringBuilder sb = {0};
    const char *render = testRender(root, &sb, mode);
    CHECK(0 == strcmp(render, result));
    free(sb.ptr);
}

static JsonDocument *createTestDocument(const char *text)
{
    struct JsonParser parser;
    testSetAllocator(&parser);
    JsonDocument *doc = jsonRead(text, (JsonSize)strlen(text), &parser);
    CHECK(parser.status == kParseOk);
    CHECK(doc);
    return doc;
}

static void testcaseObjectGet(void)
{
    testcaseInit("object_get");
    JsonDocument *doc = createTestDocument(kQueryExample);
    JsonValue *val = jsonRoot(doc);
    JsonValue *obj = jsonContainerGet(val, 0);
    CHECK(obj);

    val = jsonContainerGet(obj, 0);
    CHECK(val);
    CHECK(jsonType(val) == kTypeString);
    CHECK(0 == memcmp(jsonString(val), "SVG Viewer", (size_t)jsonLength(val)));

    val = jsonContainerGet(obj, 1);
    CHECK(val);
    CHECK(jsonType(val) == kTypeArray);
    CHECK(jsonLength(val) == 3);

    val = jsonContainerGet(obj, 2);
    CHECK(val);
    CHECK(jsonType(val) == kTypeArray);
    CHECK(jsonLength(val) == 22);

    jsonDestroyDocument(doc);
}

static void testcaseArrayGet(void)
{
    testcaseInit("array_get");
    JsonDocument *doc = createTestDocument(kQueryExample);

    static const char *kElements[] = {
        "Open",      "OpenNew", NULL,        "ZoomIn",  "ZoomOut", "OriginalView",
        NULL,        "Quality", "Pause",     "Mute",    NULL,      "Find",
        "FindAgain", "Copy",    "CopyAgain", "CopySVG", "ViewSVG", "ViewSource",
        "SaveAs",    NULL,      "Help",      "About",
    };
    JsonValue *obj = jsonContainerGet(jsonRoot(doc), 0);
    CHECK(obj);
    JsonValue *arr = jsonContainerGet(obj, 2);
    CHECK(arr);
    for (JsonSize i = 0; i < COUNTOF(kElements); ++i) {
        JsonValue *val = jsonContainerGet(arr, i);
        if (kElements[i]) {
            CHECK(jsonType(val) == kTypeObject);
            CHECK(jsonLength(val) > 0);
            // Check the first member value.
            val = jsonContainerGet(val, 0);
            CHECK(jsonType(val) == kTypeString);
            CHECK(jsonLength(val) == (JsonSize)strlen(kElements[i]));
            CHECK(0 == memcmp(jsonString(val), kElements[i], (size_t)jsonLength(val)));
        } else {
            CHECK(jsonType(val) == kTypeNull);
        }
    }
    jsonDestroyDocument(doc);
}

static void testcaseCursorContainerRoot(void)
{
    testcaseInit("cursor_container_root");
    JsonDocument *doc = createTestDocument("[42,[true]]");
    testCheckIteration(jsonRoot(doc), "<a:2><i:42><a:1><true>", kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "<i:42><a:1>", kCursorNormal);
    jsonDestroyDocument(doc);
}

static void testcaseCursorScalarRoot(void)
{
    testcaseInit("cursor_scalar_root");
    JsonDocument *doc = createTestDocument("42");
    testCheckIteration(jsonRoot(doc), "<i:42>", kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "", kCursorNormal);
    jsonDestroyDocument(doc);

    doc = createTestDocument("\"abc\"");
    testCheckIteration(jsonRoot(doc), "<s:abc>", kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "", kCursorNormal);
    jsonDestroyDocument(doc);
}

static void testcaseCursorEmptyRoot(void)
{
    testcaseInit("cursor_empty_root");
    JsonDocument *doc = createTestDocument("{}");
    testCheckIteration(jsonRoot(doc), "<o:0>", kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "", kCursorNormal);
    jsonDestroyDocument(doc);

    doc = createTestDocument("[]");
    testCheckIteration(jsonRoot(doc), "<a:0>", kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "", kCursorNormal);
    jsonDestroyDocument(doc);
}

static void testcaseCursorNested(void)
{
    testcaseInit("cursor_nested");
    JsonDocument *doc = createTestDocument("[[[{\"a\":1}],2,3],4,[5,[{\"b\":6},[7]],[8],"
                                           "{\"c\":[[9,10]]}]]");
    testCheckIteration(jsonRoot(doc),
                       "<a:3><a:3><a:1><o:1><k:a><i:1><i:2><i:3><i:4><a:4><i:5><a:2><o:1>"
                       "<k:b><i:6><a:1><i:7><a:1><i:8><o:1><k:c><a:1><a:2><i:9><i:10>",
                       kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "<a:3><i:4><a:4>", kCursorNormal);
    jsonDestroyDocument(doc);
}

static void testcaseCursorParent(void)
{
    testcaseInit("cursor_parent");
    JsonDocument *doc = createTestDocument("[1,2,{\"a\":{\"b\":[3,[4]]}},5,6]");
    testCheckIteration(jsonRoot(doc),
                       "<a:5><i:1><i:2><o:1><k:a><o:1><k:b>"
                       "<a:2><i:3><a:1><i:4><i:5><i:6>",
                       kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "<i:1><i:2><o:1><i:5><i:6>", kCursorNormal);
    jsonDestroyDocument(doc);

    doc = createTestDocument("[1,[2,[[[[3],4],5],6]]]");
    testCheckIteration(jsonRoot(doc),
                       "<a:2><i:1><a:2><i:2><a:2><a:2><a:2>"
                       "<a:1><i:3><i:4><i:5><i:6>",
                       kCursorRecursive);
    testCheckIteration(jsonRoot(doc), "<i:1><a:2>", kCursorNormal);
    jsonDestroyDocument(doc);
}

static void testcaseReadmeExample(void)
{
    testcaseInit("readme_example");

    static const char kJson[] = "[1,[2,[3],4],5]";

    struct JsonParser parser;
    jsonParserInit(&parser, NULL);

    JsonDocument *doc = jsonRead(kJson, (JsonSize)strlen(kJson), &parser);
    CHECK(doc);
    JsonValue *root = jsonRoot(doc);
    CHECK(jsonType(root) == kTypeArray);
    CHECK(jsonLength(root) == 3);

    int64_t integer = 1;
    struct JsonCursor c;
    jsonCursorInit(root, kCursorRecursive, &c);
    while (jsonCursorIsValid(&c)) {
        if (jsonType(c.value) == kTypeInteger) {
            CHECK(jsonInteger(c.value) == integer);
            ++integer;
        }
        jsonCursorNext(&c);
    }

    JsonValue *arr = jsonContainerGet(root, 1);
    const JsonSize n = jsonWrite(NULL, 0, arr);
    char *buffer = malloc((size_t)n);
    CHECK(n == jsonWrite(buffer, n, arr));

    jsonDestroyDocument(doc);
}

int main(void)
{
    puts("* * * running document API tests * * *");

    testcaseArrayGet();
    testcaseObjectGet();
    testcaseReaderOom();
    testcaseCursorContainerRoot();
    testcaseCursorScalarRoot();
    testcaseCursorEmptyRoot();
    testcaseCursorNested();
    testcaseCursorParent();
    testcaseReadmeExample();
    testcaseLongStrings();

    checkForLeaks();

    puts("* * * passed all testcases * * *");
    return 0;
}
