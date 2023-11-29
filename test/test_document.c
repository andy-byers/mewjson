#include "test.h"
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

static void testcaseInit(const char *name)
{
    puts(name);
    sOomCounter = -1;
}

static void testcaseLongStrings()
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

static void testcaseReaderOom()
{
    testcaseInit("reader_oom");
    struct JsonParser parser;

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

static JsonValue *testFind(JsonValue *root, const char *jsonPtr)
{
    JsonValue *val;
    CHECK(kQueryOk == jsonFind(root, jsonPtr, (JsonSize)strlen(jsonPtr), &val));

    static char location[8192];
    const JsonSize len = jsonLocate(root, location, SIZEOF(location), val);
    CHECK(len >= 0 && len <= SIZEOF(location));
    CHECK(0 == memcmp(jsonPtr, location, len));
    return val;
}

static void testDoNotFind(JsonValue *root, const char *jsonPtr)
{
    JsonValue *val;
    CHECK(kQueryNotFound == jsonFind(root, jsonPtr, (JsonSize)strlen(jsonPtr), &val));
    CHECK(!val);
}

static void testInvalidPointer(JsonValue *root, const char *jsonPtr)
{
    JsonValue *val;
    CHECK(kQueryPathInvalid == jsonFind(root, jsonPtr, (JsonSize)strlen(jsonPtr), &val));
    CHECK(!val);
}

typedef void (*EachCallback)(JsonValue *, JsonValue *);

static void testForEachValue(JsonValue *root, EachCallback callback)
{
    struct JsonCursor c;
    jsonCursorInit(root, kCursorRecursive, &c);
    while (jsonCursorIsValid(&c)) {
        callback(root, c.value);
        jsonCursorNext(&c);
    }
}

static JsonDocument *createTestDocument(const char *text)
{
    struct JsonParser parser;
    JsonDocument *doc = jsonRead(text, (JsonSize)strlen(text), &parser);
    CHECK(parser.status == kParseOk);
    CHECK(doc);
    return doc;
}

static void testcaseObjectGet()
{
    testcaseInit("object_get");
    JsonDocument *doc = createTestDocument(kQueryExample);
    JsonValue *val = jsonRoot(doc);
    JsonValue *obj = jsonContainerGet(val, 0);
    CHECK(obj);

    val = jsonContainerGet(obj, 0);
    CHECK(val);
    CHECK(jsonType(val) == kTypeString);
    CHECK(0 == memcmp(jsonString(val), "SVG Viewer", jsonLength(val)));

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

static void testcaseArrayGet()
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
            CHECK(0 == memcmp(jsonString(val), kElements[i], jsonLength(val)));
        } else {
            CHECK(jsonType(val) == kTypeNull);
        }
    }
    jsonDestroyDocument(doc);
}

static void testCheckLocations(JsonValue *root, JsonValue *value)
{
    char pointer[4096];
    // Determine the JSON pointer that refers to the target value, relative to the given root.
    const JsonSize length = jsonLocate(root, NULL, 0, value);
    CHECK(length >= 0 && length < SIZEOF(pointer));
    CHECK(length == jsonLocate(root, pointer, SIZEOF(pointer), value));

    JsonValue *result;
    // Make sure the generated JSON pointer can be used to find the target value again.
    const enum JsonQueryStatus e = jsonFind(root, pointer, length, &result);
    CHECK(e == kQueryOk);
    CHECK(result == value);

    // Roundtrip the pointer and make sure it matches.
    char pointer2[4096];
    CHECK(length == jsonLocate(root, pointer2, length, value));
    CHECK(0 == memcmp(pointer, pointer2, length));
}

static void testcaseFindExistent()
{
    testcaseInit("find_existent");
    JsonDocument *doc = createTestDocument(kQueryExample);
    JsonValue *root = jsonRoot(doc);

    // Check all locations in the document.
    testForEachValue(root, testCheckLocations);

    JsonValue *res = root;
    JsonValue *val = testFind(root, "");
    CHECK(val == res);

    res = jsonContainerGet(res, 0);
    val = testFind(root, "/menu");
    CHECK(val == res);

    res = jsonContainerGet(res, 2);
    val = testFind(root, "/menu/items");
    CHECK(val == res);

    res = jsonContainerGet(res, 21);
    val = testFind(root, "/menu/items/21");
    CHECK(val == res);

    res = jsonContainerGet(res, 1);
    val = testFind(root, "/menu/items/21/label");
    CHECK(val == res);
    CHECK(0 == memcmp("About Adobe CVG Viewer...", jsonString(val), jsonLength(val)));

    // Object members may have empty keys.
    val = testFind(root, "/menu/"); // tokens = ("menu", "")
    CHECK(jsonType(val) == kTypeArray);
    CHECK(jsonLength(val) == 3);
    val = testFind(root, "/menu//1/b/1/0"); // tokens = ("menu", "", 1, "b", 1, 0)
    CHECK(jsonType(val) == kTypeInteger);
    CHECK(jsonInteger(val) == 3);

    jsonDestroyDocument(doc);
}

static void testcaseFindNonexistent()
{
    testcaseInit("find_nonexistent");
    JsonDocument *doc = createTestDocument(kQueryExample);
    JsonValue *root = jsonRoot(doc);

    testDoNotFind(root, "/items");
    testDoNotFind(root, "/men");
    testDoNotFind(root, "/menu ");
    testDoNotFind(root, "/menv");
    testDoNotFind(root, "/menu/item");
    testDoNotFind(root, "/menu/items ");
    testDoNotFind(root, "/menu/iten");
    testDoNotFind(root, "/menu/items/");
    testDoNotFind(root, "/menu/items//id");
    testDoNotFind(root, "/menu/items/22");
    testDoNotFind(root, "/menu/items/-");
    testDoNotFind(root, "/menu/items/2305843009213693951");
    testDoNotFind(root, "/menu/items/2305843009213693952");
    testDoNotFind(root, "/menu/items/2305843009213693953");
    testDoNotFind(root, "/menu/items/9999999999999999999");
    testDoNotFind(root, "/menu/header/0");
    testDoNotFind(root, "/menu/header/nonexistent");

    jsonDestroyDocument(doc);
}

static void testcaseFindRelative()
{
    testcaseInit("find_relative");
    JsonDocument *doc = createTestDocument(kQueryExample);
    // Find the array at JSON pointer "/menu/items".
    JsonValue *root = jsonRoot(doc);
    root = jsonContainerGet(root, 0);
    root = jsonContainerGet(root, 2);
    CHECK(root);
    CHECK(jsonType(root) == kTypeArray);
    CHECK(jsonLength(root) == 22);

    // Check all locations in the array.
    testForEachValue(root, testCheckLocations);

    JsonValue *res = root;
    JsonValue *val = testFind(root, "");
    CHECK(val == res);

    res = jsonContainerGet(res, 5);
    val = testFind(root, "/5");
    CHECK(val == res);

    res = jsonContainerGet(res, 1);
    val = testFind(root, "/5/label");
    CHECK(val == res);
    CHECK(0 == memcmp("Original View", jsonString(val), jsonLength(val)));

    jsonDestroyDocument(doc);
}

static void testcaseFindScalarRoot()
{
    testcaseInit("find_scalar_root");
    JsonDocument *doc = createTestDocument("123");
    JsonValue *root = jsonRoot(doc);

    testForEachValue(root, testCheckLocations);

    JsonValue *val = testFind(root, "");
    CHECK(jsonType(val) == kTypeInteger);
    CHECK(jsonInteger(val) == 123);

    testDoNotFind(root, "/0");
    testDoNotFind(root, "/"); // Empty key token
    testDoNotFind(root, "/key");

    jsonDestroyDocument(doc);
}

static void testcaseFindWithEscapes()
{
    testcaseInit("find_with_escapes");
    JsonDocument *doc = createTestDocument("{\"x~/\":{\"~/x\":{\"/x~\":{\"~1\":42}}}}");
    JsonValue *root = jsonRoot(doc);

    testForEachValue(root, testCheckLocations);

    //  Before | After
    // --------|-------
    //  ~0     | ~
    //  ~1     | /
    //  ~01    | ~1
    JsonValue *val = testFind(root, "/x~0~1/~0~1x/~1x~0/~01");
    CHECK(jsonType(val) == kTypeInteger);
    CHECK(jsonInteger(val) == 42);
    jsonDestroyDocument(doc);
}

static void testcaseFindLongKeys()
{
    testcaseInit("find_long_keys");
    char longKey[1024];
    memset(longKey, '*', SIZEOF(longKey));

    char *jsonText = testMalloc(SIZEOF(longKey) * 3);
    char *text = jsonText;
    *text++ = '{';
    *text++ = '"';
    memcpy(text, longKey, SIZEOF(longKey));
    text += SIZEOF(longKey);
    *text++ = '"';
    *text++ = ':';
    *text++ = '{';
    *text++ = '"';
    memcpy(text, longKey, SIZEOF(longKey));
    text += SIZEOF(longKey);
    *text++ = '"';
    *text++ = ':';
    *text++ = '4';
    *text++ = '2';
    *text++ = '}';
    *text++ = '}';
    *text++ = '\0';

    JsonDocument *doc = createTestDocument(jsonText);
    JsonValue *root = jsonRoot(doc);

    testForEachValue(root, testCheckLocations);

    char *jsonPtr = testMalloc(SIZEOF(longKey) * 2 + 3);
    char *ptr = jsonPtr;
    *ptr++ = '/';
    memcpy(ptr, longKey, SIZEOF(longKey));
    ptr += SIZEOF(longKey);
    *ptr++ = '/';
    memcpy(ptr, longKey, SIZEOF(longKey));
    ptr += SIZEOF(longKey);
    *ptr++ = '\0';

    JsonValue *val = testFind(root, jsonPtr);
    CHECK(jsonType(val) == kTypeInteger);
    CHECK(jsonInteger(val) == 42);
    jsonDestroyDocument(doc);
    testFree(jsonPtr);
    testFree(jsonText);
}

static void testcaseFindInvalidPointer()
{
    testcaseInit("find_invalid_pointer");
    JsonDocument *doc = createTestDocument(kQueryExample);
    JsonValue *root = jsonRoot(doc);

    // Missing leading '/'
    testInvalidPointer(root, "menu");

    // Corrupted array index
    testInvalidPointer(root, "/menu/items/01");
    testInvalidPointer(root, "/menu/items/1x");

    jsonDestroyDocument(doc);
}

static void testcaseCursorContainerRoot()
{
    testcaseInit("cursor_container_root");
    JsonDocument *doc = createTestDocument("[42,[true]]");
    JsonValue *root = jsonRoot(doc);

    struct JsonCursor c;
    jsonCursorInit(root, kCursorNormal, &c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeInteger);
    CHECK(jsonInteger(c.value) == 42);
    jsonCursorNext(&c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeArray);
    CHECK(jsonLength(c.value) == 1);
    jsonCursorNext(&c);
    CHECK(!jsonCursorIsValid(&c));

    jsonCursorInit(root, kCursorRecursive, &c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeArray);
    CHECK(jsonLength(c.value) == 2);
    jsonCursorNext(&c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeInteger);
    CHECK(jsonInteger(c.value) == 42);
    jsonCursorNext(&c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeArray);
    CHECK(jsonLength(c.value) == 1);
    jsonCursorNext(&c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeBoolean);
    CHECK(jsonBoolean(c.value));
    jsonCursorNext(&c);
    CHECK(!jsonCursorIsValid(&c));

    jsonDestroyDocument(doc);
}

static void testcaseCursorNonContainerRoot()
{
    testcaseInit("cursor_non_container_root");
    JsonDocument *doc = createTestDocument("42");
    JsonValue *root = jsonRoot(doc);

    struct JsonCursor c;
    jsonCursorInit(root, kCursorNormal, &c);
    CHECK(!jsonCursorIsValid(&c));

    jsonCursorInit(root, kCursorRecursive, &c);
    CHECK(jsonCursorIsValid(&c));
    CHECK(jsonType(c.value) == kTypeInteger);
    CHECK(jsonInteger(c.value) == 42);

    jsonDestroyDocument(doc);
}

int main()
{
    jsonSetAllocator((struct JsonAllocator){
        testMalloc,
        testRealloc,
        testFree,
    });

    puts("* * * running document API tests * * *");

    testcaseArrayGet();
    testcaseObjectGet();
    testcaseFindExistent();
    testcaseFindNonexistent();
    testcaseFindRelative();
    testcaseFindInvalidPointer();
    testcaseFindScalarRoot();
    testcaseFindWithEscapes();
    testcaseFindLongKeys();
    testcaseReaderOom();
    testcaseCursorContainerRoot();
    testcaseCursorNonContainerRoot();
    testcaseLongStrings();

    checkForLeaks();

    puts("* * * passed all testcases * * *");
    return 0;
}
