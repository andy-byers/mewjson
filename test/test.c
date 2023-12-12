#include "mewjson.h"
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

struct TestAllocatorState {
    JsonSize bytesUsed;
    int oomCounter;
} sState;

void checkForLeaks(void)
{
    CHECK(sState.bytesUsed == 0);
}

// Modified from P-Gn's answer to https://stackoverflow.com/questions/4915462.
JsonBool areRealsEqual(double x, double y)
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

static void *testMalloc(void *ctx, size_t size)
{
    struct TestAllocatorState *state = ctx;
    if (state->oomCounter == 0) {
        return NULL;
    } else if (state->oomCounter > 0) {
        --state->oomCounter;
    }
    JsonSize *ptr = malloc(size);
    if (ptr) {
        state->bytesUsed += (JsonSize)size;
    }
    return ptr;
}

static void *testRealloc(void *ctx, void *ptr, size_t oldSize, size_t newSize)
{
    struct TestAllocatorState *state = ctx;
    if (state->oomCounter == 0) {
        return NULL;
    } else if (state->oomCounter > 0) {
        --state->oomCounter;
    }
    JsonSize *newPtr = realloc(ptr, newSize);
    if (newPtr) {
        state->bytesUsed -= (JsonSize)oldSize;
        state->bytesUsed += (JsonSize)newSize;
    }
    return newPtr;
}

static void testFree(void *ctx, void *ptr, size_t size)
{
    if (ptr) {
        CHECK(size > 0);
        struct TestAllocatorState *state = ctx;
        state->bytesUsed -= (JsonSize)size;
        CHECK(state->bytesUsed >= 0);
        free(ptr);
    }
}

static struct JsonAllocator sAllocator = {
    .ctx = &sState,
    .malloc = testMalloc,
    .realloc = testRealloc,
    .free = testFree,
};

static const char *kNoTestNames[] = {
    "n_array_1_true_without_comma",
    "n_array_a_invalid_utf8",
    "n_array_colon_instead_of_comma",
    "n_array_comma_after_close",
    "n_array_comma_and_number",
    "n_array_double_comma",
    "n_array_double_extra_comma",
    "n_array_extra_close",
    "n_array_extra_comma",
    "n_array_incomplete",
    "n_array_incomplete_invalid_value",
    "n_array_inner_array_no_comma",
    "n_array_invalid_utf8",
    "n_array_items_separated_by_semicolon",
    "n_array_just_comma",
    "n_array_just_minus",
    "n_array_missing_value",
    "n_array_newlines_unclosed",
    "n_array_number_and_comma",
    "n_array_number_and_several_commas",
    "n_array_spaces_vertical_tab_formfeed",
    "n_array_star_inside",
    "n_array_unclosed",
    "n_array_unclosed_trailing_comma",
    "n_array_unclosed_with_new_lines",
    "n_array_unclosed_with_object_inside",
    "n_incomplete_false",
    "n_incomplete_null",
    "n_incomplete_true",
    "n_multidigit_number_then_00",
    "n_number_++",
    "n_number_+1",
    "n_number_+Inf",
    "n_number_-01",
    "n_number_-1.0.",
    "n_number_-2.",
    "n_number_-NaN",
    "n_number_.-1",
    "n_number_.2e-3",
    "n_number_0.1.2",
    "n_number_0.3e+",
    "n_number_0.3e",
    "n_number_0.e1",
    "n_number_0_capital_E+",
    "n_number_0_capital_E",
    "n_number_0e+",
    "n_number_0e",
    "n_number_1.0e+",
    "n_number_1.0e-",
    "n_number_1.0e",
    "n_number_1_000",
    "n_number_1eE2",
    "n_number_2.e+3",
    "n_number_2.e-3",
    "n_number_2.e3",
    "n_number_9.e+",
    "n_number_Inf",
    "n_number_NaN",
    "n_number_U+FF11_fullwidth_digit_one",
    "n_number_expression",
    "n_number_hex_1_digit",
    "n_number_hex_2_digits",
    "n_number_infinity",
    "n_number_invalid+-",
    "n_number_invalid-negative-real",
    "n_number_invalid-utf-8-in-bigger-int",
    "n_number_invalid-utf-8-in-exponent",
    "n_number_invalid-utf-8-in-int",
    "n_number_minus_infinity",
    "n_number_minus_sign_with_trailing_garbage",
    "n_number_minus_space_1",
    "n_number_neg_int_starting_with_zero",
    "n_number_neg_real_without_int_part",
    "n_number_neg_with_garbage_at_end",
    "n_number_real_garbage_after_e",
    "n_number_real_with_invalid_utf8_after_e",
    "n_number_real_without_fractional_part",
    "n_number_starting_with_dot",
    "n_number_with_alpha",
    "n_number_with_alpha_char",
    "n_number_with_leading_zero",
    "n_object_bad_value",
    "n_object_bracket_key",
    "n_object_comma_instead_of_colon",
    "n_object_double_colon",
    "n_object_emoji",
    "n_object_garbage_at_end",
    "n_object_key_with_single_quotes",
    "n_object_lone_continuation_byte_in_key_and_trailing_comma",
    "n_object_missing_colon",
    "n_object_missing_key",
    "n_object_missing_semicolon",
    "n_object_missing_value",
    "n_object_no-colon",
    "n_object_non_string_key",
    "n_object_non_string_key_but_huge_number_instead",
    "n_object_repeated_null_null",
    "n_object_several_trailing_commas",
    "n_object_single_quote",
    "n_object_trailing_comma",
    "n_object_two_commas_in_a_row",
    "n_object_unquoted_key",
    "n_object_unterminated-value",
    "n_object_with_single_string",
    "n_object_with_trailing_garbage",
    "n_single_space",
    "n_string_1_surrogate_then_escape",
    "n_string_1_surrogate_then_escape_u",
    "n_string_1_surrogate_then_escape_u1",
    "n_string_1_surrogate_then_escape_u1x",
    "n_string_accentuated_char_no_quotes",
    "n_string_backslash_00",
    "n_string_escape_x",
    "n_string_escaped_backslash_bad",
    "n_string_escaped_ctrl_char_tab",
    "n_string_escaped_emoji",
    "n_string_incomplete_escape",
    "n_string_incomplete_escaped_character",
    "n_string_incomplete_surrogate",
    "n_string_incomplete_surrogate_escape_invalid",
    "n_string_invalid-utf-8-in-escape",
    "n_string_invalid_backslash_esc",
    "n_string_invalid_unicode_escape",
    "n_string_invalid_utf8_after_escape",
    "n_string_leading_uescaped_thinspace",
    "n_string_no_quotes_with_bad_escape",
    "n_string_single_doublequote",
    "n_string_single_quote",
    "n_string_single_string_no_double_quotes",
    "n_string_start_escape_unclosed",
    "n_string_unescaped_crtl_char",
    "n_string_unescaped_newline",
    "n_string_unescaped_tab",
    "n_string_unicode_CapitalU",
    "n_string_with_trailing_garbage",
    "n_structure_100000_opening_arrays",
    "n_structure_U+2060_word_joined",
    "n_structure_UTF8_BOM_no_data",
    "n_structure_angle_bracket_.",
    "n_structure_angle_bracket_null",
    "n_structure_array_trailing_garbage",
    "n_structure_array_with_extra_array_close",
    "n_structure_array_with_unclosed_string",
    "n_structure_ascii-unicode-identifier",
    "n_structure_capitalized_True",
    "n_structure_close_unopened_array",
    "n_structure_comma_instead_of_closing_brace",
    "n_structure_double_array",
    "n_structure_end_array",
    "n_structure_incomplete_UTF8_BOM",
    "n_structure_lone-invalid-utf-8",
    "n_structure_lone-open-bracket",
    "n_structure_no_data",
    "n_object_trailing_comment_open",
    "n_object_trailing_comment_slash_open",
    "n_object_trailing_comment_slash_open_incomplete",
    "n_structure_null-byte-outside-string",
    "n_structure_number_with_trailing_garbage",
    "n_structure_object_followed_by_closing_object",
    "n_structure_object_unclosed_no_value",
    "n_structure_object_with_trailing_garbage",
    "n_structure_open_array_apostrophe",
    "n_structure_open_array_comma",
    "n_structure_open_array_object",
    "n_structure_open_array_open_object",
    "n_structure_open_array_open_string",
    "n_structure_open_array_string",
    "n_structure_open_object",
    "n_structure_open_object_close_array",
    "n_structure_open_object_comma",
    "n_structure_open_object_open_array",
    "n_structure_open_object_open_string",
    "n_structure_open_object_string_with_apostrophes",
    "n_structure_open_open",
    "n_structure_single_eacute",
    "n_structure_single_star",
    "n_structure_trailing_#",
    "n_structure_uescaped_LF_before_string",
    "n_structure_object_with_comment",
    "n_object_trailing_comment",
    "n_structure_unclosed_array",
    "n_structure_unclosed_array_partial_null",
    "n_structure_unclosed_array_unfinished_false",
    "n_structure_unclosed_array_unfinished_true",
    "n_structure_unclosed_object",
    "n_structure_unicode-identifier",
    "n_structure_whitespace_U+2060_word_joiner",
    "n_structure_whitespace_formfeed",
    "fail02",
    "fail03",
    "fail04",
    "fail05",
    "fail06",
    "fail07",
    "fail08",
    "fail09",
    "fail10",
    "fail11",
    "fail12",
    "fail13",
    "fail14",
    "fail15",
    "fail16",
    "fail17",
    "fail19",
    "fail20",
    "fail21",
    "fail22",
    "fail23",
    "fail24",
    "fail25",
    "fail26",
    "fail27",
    "fail28",
    "fail29",
    "fail30",
    "fail31",
    "fail32",
    "fail33",
    "fail34",
    "fail35",
    "fail36",
    "fail37",
    "fail38",
    "fail42",
    "fail43",
    "fail44",
    "fail45",
    "fail46",
    "fail47",
    "fail48",
    "fail49",
    "fail50",
    "fail51",
    "fail52",
    "fail53",
    "fail54",
    "fail55",
    "fail56",
    "fail57",
    "fail58",
    "fail59",
    "fail61",
    "fail62",
    "fail63",
    "fail64",
    "fail65",
    "fail66",
    "fail67",
    "fail68",
    "fail69",
    "fail70",
    "fail71",
    "fail72",
    "fail74",
    "fail75",
    "fail76",
    "fail77",
    "fail78",
    "fail79",
    "fail80",
    "fail81",
    "fail82",

    // Invalid surrogate pair.
    "i_object_key_lone_2nd_surrogate",
    "i_string_1st_surrogate_but_2nd_missing",
    "i_string_1st_valid_surrogate_2nd_invalid",
    "i_string_incomplete_surrogate_and_escape_valid",
    "i_string_incomplete_surrogate_pair",
    "i_string_incomplete_surrogates_escape_valid",
    "i_string_invalid_lonely_surrogate",
    "i_string_invalid_surrogate",
    "i_string_inverted_surrogates_U+1D11E",
    "i_string_lone_second_surrogate",

    // Invalid UTF-8.
    "i_string_UTF-8_invalid_sequence",
    "i_string_invalid_utf-8",
    "i_string_iso_latin_1",
    "i_string_lone_utf8_continuation_byte",
    "i_string_overlong_sequence_6_bytes",
    "i_string_overlong_sequence_6_bytes_null",
    "i_string_truncated-utf-8",
    "i_string_UTF-16LE_with_BOM",
    "i_string_utf16BE_no_BOM",
    "i_string_utf16LE_no_BOM",
    "i_string_UTF8_surrogate_U+D800",
    "i_string_overlong_sequence_2_bytes",
    "i_string_not_in_unicode_range",

    // Doesn't support BOM.
    "i_structure_UTF-8_BOM_empty_object",
};

static const char *kYesTestNames[] = {
    "y_array_arraysWithSpaces",
    "y_array_empty-string",
    "y_array_empty",
    "y_array_ending_with_newline",
    "y_array_false",
    "y_array_heterogeneous",
    "y_array_null",
    "y_array_with_1_and_newline",
    "y_array_with_leading_space",
    "y_array_with_several_null",
    "y_array_with_trailing_space",
    "y_number",
    "y_number_0e+1",
    "y_number_0e1",
    "y_number_after_space",
    "y_number_double_close_to_zero",
    "y_number_int_with_exp",
    "y_number_minus_zero",
    "y_number_negative_int",
    "y_number_negative_one",
    "y_number_negative_zero",
    "y_number_real_capital_e",
    "y_number_real_capital_e_neg_exp",
    "y_number_real_capital_e_pos_exp",
    "y_number_real_exponent",
    "y_number_real_fraction_exponent",
    "y_number_real_neg_exp",
    "y_number_real_pos_exponent",
    "y_number_simple_int",
    "y_number_simple_real",
    "y_object",
    "y_object_basic",
    "y_object_duplicated_key",
    "y_object_duplicated_key_and_value",
    "y_object_empty",
    "y_object_empty_key",
    "y_object_escaped_null_in_key",
    "y_object_extreme_numbers",
    "y_object_long_strings",
    "y_object_simple",
    "y_object_string_unicode",
    "y_object_with_newlines",
    "y_string_1_2_3_bytes_UTF-8_sequences",
    "y_string_accepted_surrogate_pair",
    "y_string_accepted_surrogate_pairs",
    "y_string_allowed_escapes",
    "y_string_backslash_and_u_escaped_zero",
    "y_string_backslash_doublequotes",
    "y_string_comments",
    "y_string_double_escape_a",
    "y_string_double_escape_n",
    "y_string_escaped_control_character",
    "y_string_escaped_noncharacter",
    "y_string_in_array",
    "y_string_in_array_with_leading_space",
    "y_string_last_surrogates_1_and_2",
    "y_string_nbsp_uescaped",
    "y_string_nonCharacterInUTF-8_U+10FFFF",
    "y_string_nonCharacterInUTF-8_U+FFFF",
    "y_string_null_escape",
    "y_string_one-byte-utf-8",
    "y_string_pi",
    "y_string_reservedCharacterInUTF-8_U+1BFFF",
    "y_string_simple_ascii",
    "y_string_space",
    "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF",
    "y_string_three-byte-utf-8",
    "y_string_two-byte-utf-8",
    "y_string_u+2028_line_sep",
    "y_string_u+2029_par_sep",
    "y_string_uEscape",
    "y_string_uescaped_newline",
    "y_string_unescaped_char_delete",
    "y_string_unicode",
    "y_string_unicodeEscapedBackslash",
    "y_string_unicode_2",
    "y_string_unicode_U+10FFFE_nonchar",
    "y_string_unicode_U+1FFFE_nonchar",
    "y_string_unicode_U+200B_ZERO_WIDTH_SPACE",
    "y_string_unicode_U+2064_invisible_plus",
    "y_string_unicode_U+FDD0_nonchar",
    "y_string_unicode_U+FFFE_nonchar",
    "y_string_unicode_escaped_double_quote",
    "y_string_utf8",
    "y_string_with_del_character",
    "y_structure_lonely_false",
    "y_structure_lonely_int",
    "y_structure_lonely_negative_real",
    "y_structure_lonely_null",
    "y_structure_lonely_string",
    "y_structure_lonely_true",
    "y_structure_string_empty",
    "y_structure_trailing_newline",
    "y_structure_true_in_array",
    "y_structure_whitespace_array",
    "pass01",
    "pass02",
    "pass03",
    "pass04",
    "pass05",
    "pass06",
    "pass07",
    "pass08",
    "pass09",
    "pass10",
    "pass11",
    "pass12",
    "pass13",
    "pass14",
    "pass15",
    "pass16",
    "pass17",
    "pass18",
    "pass19",
    "pass20",
    "pass21",
    "pass22",
    "pass23",
    "pass24",
    "pass25",
    "pass26",
    "pass27",

    // Overflowing integers become reals.
    "i_number_double_huge_neg_exp",
    "i_number_huge_exp",
    "i_number_neg_int_huge_exp",
    "i_number_pos_double_huge_exp",
    "i_number_real_neg_overflow",
    "i_number_real_pos_overflow",
    "i_number_real_underflow",
    "i_number_too_big_neg_int",
    "i_number_too_big_pos_int",
    "i_number_very_big_negative_int",

    // Supports more than 500 nested arrays by default.
    "i_structure_500_nested_arrays",

    // These 2 have extremely large real numbers. This implementation lets them become
    // +/-Inf. jsonStringify() will write them as nulls.
    "fail60",
    "fail73",
};

// clang-format off
static struct Testcase {
    const char *name;
    const char *input;
    JsonSize inputLen;
    enum TestcaseType {
        kTestcaseNo,
        kTestcaseYes,
    } type;
} sInternalTestcases[] = {
#define YES(a, b) {.name = CAT(y_, a), .input = b, .inputLen = (JsonSize)SIZEOF(b) - 1, .type = kTestcaseYes},
    YES(structure_complicated, "[{\"a\":[{}]},{\"b\":[{\"c\":[[],[],[true]],\"d\":null}]}]")
    YES(array_nested, "[1,[2,[3,4],5,[6,7],8],9,[10,[11,12],13,[14,15],16],17]")
    YES(number_simple_int, "123")
    YES(number_negative_int, "-123")
    YES(number_after_space, " 4")
    YES(number_negative_one, "-1")
    YES(number_negative_zero, "-0")
    YES(number_integer_min, "-9223372036854775808")
    YES(number_integer_max, "9223372036854775807")
    YES(number_integer_small, "-9223372036854775807")
    YES(structure_lonely_negative_real, "-0.1")
    YES(number_simple_real, "123.456789")
    YES(number_real_positive_exponent, "1e+2")
    YES(number_real_negative_exponent, "1e-2")
    YES(number_real_small, "-1.0e+28")
    YES(number_real_large_1, "1.0e+28")
    YES(number_real_large_2, "123e45")
    YES(number_real_large_3, "123e65")
    YES(number_real_close_to_zero,
        "-0.000000000000000000000000000000000000000000000000000000000000000000000000000001")
    YES(number_integer_with_exponent, "20e1") // Becomes a real
    YES(number_real_capital_e_pos_exponent, "1E+2")
    YES(number_real_capital_e, "1E22")
    YES(number_real_fraction_exponent, "123.456e78")
    YES(number_real_capital_e_negative_exponent, "1E-2")
    YES(number_0e+1, "0e+1")
    YES(number_0e-1, "0e-1")
    YES(number_0e1, "0e1")
    YES(number_underflowing_integer_becomes_real_1, "-9223372036854775809")
    YES(number_underflowing_integer_becomes_real_2, "-92233720368547758080")
    YES(number_overflowing_integer_becomes_real_1, "9223372036854775808")
    YES(number_overflowing_integer_becomes_real_2, "9223372036854775809")
    YES(number_overflowing_integer_becomes_real_3, "92233720368547758199")
    YES(number_exponent_with_leading_zero, "1.23e01")
    YES(number_exponent_with_leading_zero_and_sign, "1.23e+01")
    YES(number_tiny_negative_real, "-1.0e-123")
    YES(number_negative_zero, "-0.0")

#define NO(a, b) {.name = CAT(n_, a), .input = b, .inputLen = (JsonSize)SIZEOF(b) - 1, .type = kTestcaseNo},
    NO(string_missing_high_surrogate, "\"\\uDC00\"")
    NO(object_with_trailing_comment, "{\"a\":\"b\"}/*comment*/")
    NO(object_with_leading_comment, "/*comment*/{\"a\":\"b\"}")
    NO(number_with_junk_at_end, "1234567*")
    NO(number_very_long_with_junk_at_end, "123456789012345678901234567890.0e*")
    NO(string_backslash_00, "[\"\\0\"]")
    NO(structure_lonely_array_end, "]")
    NO(structure_lonely_object_end, "}")
    NO(structure_embedded_EOF, "[\xFF]")
    NO(structure_EOF_at_end, "[]\xFF")
    NO(structure_EOF_at_end2, "[]\xFF\xFF")
    NO(structure_mismatched_containers, "[}")
    NO(structure_mismatched_containers2, "[[1,2,true,{\"a\":\"b\"}]}")
    NO(structure_mismatched_containers3, "{]")
    NO(string_with_EOF, "\"\xFF\"")
};
// clang-format on

enum {
    kTypeKey = kTypeCount,
};

struct TestcaseNode {
    int type;
    JsonSize size;
    union {
        const char *string;
        int64_t integer;
        double real;
        JsonBool boolean;
    };
};

// clang-format off
#define MAKE_KEY_WITH_LEN(s, n) (struct TestcaseNode){.type = kTypeString, .size = (n), .string = (s)}
#define MAKE_STRING_WITH_LEN(s, n) (struct TestcaseNode){.type = kTypeString, .size = (n), .string = (s)}
#define MAKE_KEY(s) MAKE_KEY_WITH_LEN(s, (JsonSize)strlen(s))
#define MAKE_STRING(s) MAKE_STRING_WITH_LEN(s, (JsonSize)strlen(s))
#define MAKE_INTEGER(i) (struct TestcaseNode){.type = kTypeInteger, .integer = (i)}
#define MAKE_REAL(r) (struct TestcaseNode){.type = kTypeReal, .real = (r)}
#define MAKE_BOOLEAN(b) (struct TestcaseNode){.type = kTypeBoolean, .boolean = (b)}
#define MAKE_NULL() (struct TestcaseNode){.type = kTypeNull}
#define MAKE_OBJECT(n) (struct TestcaseNode){.type = kTypeObject, .size = (n)}
#define MAKE_ARRAY(n) (struct TestcaseNode){.type = kTypeArray, .size = (n)}
// clang-format on

static void sanityCheckRealEquality(void)
{
    CHECK(areRealsEqual(0.0, -0.0));
    CHECK(areRealsEqual(1.0, 1.0));
    CHECK(areRealsEqual(1.0, 1.0 + 1e-20)); // Close enough
    CHECK(areRealsEqual(INFINITY, INFINITY));
    CHECK(areRealsEqual(-INFINITY, -INFINITY));

    CHECK(!areRealsEqual(1.0, 1.00001));
    CHECK(!areRealsEqual(1.0, 1.0 + 1e-10));
    CHECK(!areRealsEqual(INFINITY, -INFINITY));
    CHECK(!areRealsEqual(NAN, NAN));
}

static void testParserInit(struct JsonParser *parser)
{
    jsonParserInit(parser, &sAllocator);
}

static void assertRoundtripMatches(JsonValue *root, struct JsonParser *parser)
{
    const JsonSize n = jsonStringify(NULL, 0, root);
    char *result = malloc((size_t)n);
    CHECK(n == jsonStringify(result, n, root));

    JsonDocument *doc2 = jsonParse(result, n, parser);
    CHECK(doc2);

    char *result2 = malloc((size_t)n);
    CHECK(n == jsonStringify(result2, n, jsonRoot(doc2)));
    jsonDestroyDocument(doc2);

    // Written JSON must match exactly.
    CHECK(0 == memcmp(result, result2, (size_t)n));
}

static void testCheckRoundtrip(JsonValue *value)
{
    struct JsonParser parser;
    testParserInit(&parser);
    assertRoundtripMatches(value, &parser);
}

static void checkValueEqualsNode(struct JsonCursor *c, const struct TestcaseNode *node)
{
    JsonValue *value = jsonCursorValue(c);
    // Roundtrip this node by itself.
    testCheckRoundtrip(value);
    if (node->type == kTypeKey) {
        CHECK(kTypeString == jsonType(value));
        CHECK(kTypeObject == jsonType(jsonCursorParent(c)));
    } else {
        CHECK(node->type == (int)jsonType(value));
    }
    switch (node->type) {
        case kTypeKey:
        case kTypeString: {
            JsonSize len;
            const char *str = jsonString(value, &len);
            CHECK(len == node->size);
            CHECK(0 == memcmp(str, node->string, (size_t)len));
            break;
        }
        case kTypeInteger:
            CHECK(jsonInteger(value) == node->integer);
            break;
        case kTypeReal:
            CHECK(areRealsEqual(jsonReal(value), node->real));
            break;
        case kTypeBoolean:
            CHECK(jsonBoolean(value) == node->boolean);
            break;
        case kTypeNull:
            break;
        default: // kTypeObject || kTypeArray
            CHECK(jsonContainerLength(value) == node->size);
    }
}

static void assertSubTreeEquals(struct JsonCursor *c, const struct TestcaseNode *nodes, JsonSize n)
{
    for (JsonSize i = 0; i < n; ++i) {
        const struct TestcaseNode *node = &nodes[i];
        CHECK(jsonCursorIsValid(c));
        checkValueEqualsNode(c, node);
        jsonCursorNext(c);
    }
}

static void assertDocumentEquals(JsonDocument *doc, const struct TestcaseNode *nodes, JsonSize n,
                                 enum JsonCursorMode mode)
{
    CHECK(doc);
    struct JsonCursor c;
    jsonCursorInit(jsonRoot(doc), mode, &c);
    assertSubTreeEquals(&c, nodes, n);

    JsonDocument *doc2 = jsonCloneDocument(doc);
    CHECK(doc2);
    jsonCursorInit(jsonRoot(doc2), mode, &c);
    assertSubTreeEquals(&c, nodes, n);
    jsonDestroyDocument(doc2);
}

static void assertJsonEquals(const char *json, JsonSize length, const struct TestcaseNode *nodes,
                             JsonSize n, enum JsonCursorMode mode)
{
    struct JsonParser parser;
    testParserInit(&parser);
    JsonDocument *doc = jsonParse(json, length, &parser);
    assertDocumentEquals(doc, nodes, n, mode);
    jsonDestroyDocument(doc);
}

static void assertInvalidJson(const char *text, JsonSize length)
{
    struct JsonParser parser;
    testParserInit(&parser);
    JsonDocument *doc = jsonParse(text, length, &parser);
    CHECK(!doc);
    CHECK(parser.status > kStatusNoMemory);
}

#define ASSERT_SUB_TREE_EQUALS(c, a) assertSubTreeEquals(c, a, COUNTOF(a))
#define ASSERT_DOCUMENT_EQUALS_R(d, a) assertDocumentEquals(d, a, COUNTOF(a), kCursorRecursive)
#define ASSERT_DOCUMENT_EQUALS_N(d, a) assertDocumentEquals(d, a, COUNTOF(a), kCursorNormal)
#define ASSERT_JSON_EQUALS_R(j, a)                                                                 \
    assertJsonEquals(j, SIZEOF(j) - 1, a, COUNTOF(a), kCursorRecursive)
#define ASSERT_JSON_EQUALS_N(j, a) assertJsonEquals(j, SIZEOF(j) - 1, a, COUNTOF(a), kCursorNormal)
#define ASSERT_INVALID_JSON(j) assertInvalidJson(j, SIZEOF(j) - 1)

static void testcaseInit(const char *name)
{
    puts(name);
    checkForLeaks();
    sState = (struct TestAllocatorState){
        .oomCounter = -1,
    };
}

static const char kOomExample[] =
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
    "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[4,2]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
    "]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]";

static void testcaseOom(void)
{
    testcaseInit("reader_oom");
    struct JsonParser parser;
    testParserInit(&parser);

    int failures = 0;
    for (;; ++failures) {
        sState.oomCounter = failures;
        JsonDocument *doc = jsonParse(kOomExample, (JsonSize)strlen(kOomExample), &parser);
        if (doc) {
            jsonDestroyDocument(doc);
            break;
        }
        CHECK(parser.status == kStatusNoMemory);
    }
    // Will likely end up failing quite a bit more than 2 times. Just a sanity check.
    CHECK(failures > 2);
}

static void testcaseRun(struct Testcase *tc)
{
    testcaseInit(tc->name);

    struct JsonParser parser;
    testParserInit(&parser);
    JsonDocument *doc = jsonParse(tc->input, tc->inputLen, &parser);
    if (tc->type == kTestcaseNo) {
        CHECK(!doc);
        // Must fail for a reason other than running out of memory. OOM conditions are tested
        // separately (see TestcaseReaderOom()).
        CHECK(parser.status != kStatusOk);
        CHECK(parser.status != kStatusNoMemory);
    } else {
        CHECK(doc);
        CHECK(parser.status == kStatusOk);
        assertRoundtripMatches(jsonRoot(doc), &parser);
        JsonDocument *doc2 = jsonCloneDocument(doc);
        assertRoundtripMatches(jsonRoot(doc2), &parser);
        jsonDestroyDocument(doc2);
        jsonDestroyDocument(doc);
    }
}

static void testcaseLargeNumbers(void)
{
    testcaseInit("large_numbers");
    ASSERT_JSON_EQUALS_R("[-1e1111,1e1111]", ((struct TestcaseNode[]){
                                                 MAKE_ARRAY(2),
                                                 MAKE_REAL(-INFINITY),
                                                 MAKE_REAL(INFINITY),
                                             }));
}

static void testcaseExceedMaxDepth(void)
{
    testcaseInit("exceed_max_depth");
    static char sText[100000];
    memset(sText, '[', (size_t)SIZEOF(sText));

    struct JsonParser parser;
    testParserInit(&parser);
    CHECK(!jsonParse(sText, SIZEOF(sText), &parser));
    CHECK(kStatusExceededMaxDepth == parser.status);
}

static void testcaseParseErrors(void)
{
    testcaseInit("parse_errors");

    static const struct {
        const char *text;
        enum JsonStatus reason;
        JsonBool zeroOffset;
    } kInputs[] = {
        // Skip kStatusOk
        // Skip kStatusNoMemory
        // Skip kStatusExceededMaxDepth
        {.reason = kStatusStringInvalidEscape, .text = "\"\\xFF\""},
        {.reason = kStatusStringInvalidCodepoint, .text = "\"\\uXYZW\""},
        {.reason = kStatusStringMissingLowSurrogate, .text = "\"\\uD800\\uD800\""},
        {.reason = kStatusStringMissingHighSurrogate, .text = "\"\\uDC00\""},
        {.reason = kStatusStringUnescapedControl, .text = "\"\x01\""},
        {.reason = kStatusStringInvalidUtf8, .text = "\"\xED\xA0\x80\""},
        {.reason = kStatusLiteralInvalid, .text = "tru"},
        {.reason = kStatusNumberMissingExponent, .text = "1.23e"},
        {.reason = kStatusNumberMissingFraction, .text = "1.e23"},
        {.reason = kStatusNumberInvalid, .text = "0123"},
        {.reason = kStatusContainerNotClosed, .text = "[{\"key\":\"value\"}"},
        {.reason = kStatusContainerNotClosed, .text = "["},
        {.reason = kStatusSyntaxError, .text = "[]123"},
        {.reason = kStatusSyntaxError, .text = "", .zeroOffset = 1},
        {.reason = kStatusSyntaxError, .text = " "},
    };

    struct JsonParser parser;
    testParserInit(&parser);
    for (JsonSize i = 0; i < COUNTOF(kInputs); ++i) {
        JsonDocument *doc = jsonParse(kInputs[i].text, (JsonSize)strlen(kInputs[i].text), &parser);
        CHECK(!doc);
        CHECK(kInputs[i].reason == parser.status);
        CHECK(!parser.offset == kInputs[i].zeroOffset);
    }
}

static char *readFileToString(const char *pathname, JsonSize *lengthOut)
{
    FILE *file = fopen(pathname, "rb");
    CHECK(file);

    CHECK(0 == fseek(file, 0, SEEK_END));
    const JsonSize fileSize = ftell(file);
    CHECK(0 == fseek(file, 0, SEEK_SET));

    char *text = malloc((size_t)fileSize + 1);
    const size_t readSize = fread(text, 1, (size_t)fileSize, file);
    CHECK(readSize == (size_t)fileSize);
    *lengthOut = fileSize;
    text[fileSize] = '\0';
    fclose(file);
    return text;
}

static void runOnePassFailTest(const char *name, enum TestcaseType type)
{
    char pathBuf[COUNTOF(TESTCASE_PREFIX) + 256] = {0};
    CHECK(strlen(name) + strlen(".json") < 256);
    strcat(pathBuf, TESTCASE_PREFIX);
    strcat(pathBuf, name);
    strcat(pathBuf, ".json");

    JsonSize inputLen;
    char *text = readFileToString(pathBuf, &inputLen);
    CHECK(text);

    struct Testcase tc = {
        .name = name,
        .type = type,
        .input = text,
        .inputLen = inputLen,
    };
    testcaseRun(&tc);
    free(text);
}

static void runExternalPassFailTests(void)
{
    for (JsonSize i = 0; i < COUNTOF(kYesTestNames); ++i) {
        runOnePassFailTest(kYesTestNames[i], kTestcaseYes);
    }
    for (JsonSize i = 0; i < COUNTOF(kNoTestNames); ++i) {
        runOnePassFailTest(kNoTestNames[i], kTestcaseNo);
    }
}

static void runInternalPassFailTests(void)
{
    for (size_t i = 0; i < COUNTOF(sInternalTestcases); ++i) {
        testcaseRun(&sInternalTestcases[i]);
    }
}

static void testcaseExpectedTypes(void)
{
    testcaseInit("expected_types");

    ASSERT_JSON_EQUALS_R("[[1,2,[3,[[4]]],5],6,7]", //
                         ((struct TestcaseNode[]){
                             MAKE_ARRAY(3),
                             MAKE_ARRAY(4),
                             MAKE_INTEGER(1),
                             MAKE_INTEGER(2),
                             MAKE_ARRAY(2),
                             MAKE_INTEGER(3),
                             MAKE_ARRAY(1),
                             MAKE_ARRAY(1),
                             MAKE_INTEGER(4),
                             MAKE_INTEGER(5),
                             MAKE_INTEGER(6),
                             MAKE_INTEGER(7),
                         }));

    ASSERT_JSON_EQUALS_R("{\"boolean\":true}", //
                         ((struct TestcaseNode[]){
                             MAKE_OBJECT(2),
                             MAKE_KEY("boolean"),
                             MAKE_BOOLEAN(1),
                         }));

    ASSERT_JSON_EQUALS_R("[0,[[[1,2],3]]]", //
                         ((struct TestcaseNode[]){
                             MAKE_ARRAY(2),
                             MAKE_INTEGER(0),
                             MAKE_ARRAY(1),
                             MAKE_ARRAY(2),
                             MAKE_ARRAY(2),
                             MAKE_INTEGER(1),
                             MAKE_INTEGER(2),
                             MAKE_INTEGER(3),
                         }));

    ASSERT_JSON_EQUALS_R("[{},[],\"abc\",123,1.0,true,null]", //
                         ((struct TestcaseNode[]){
                             MAKE_ARRAY(7),
                             MAKE_OBJECT(0),
                             MAKE_ARRAY(0),
                             MAKE_STRING("abc"),
                             MAKE_INTEGER(123),
                             MAKE_REAL(1.0),
                             MAKE_BOOLEAN(1),
                             MAKE_NULL(),
                         }));

    ASSERT_JSON_EQUALS_R("[[{},[]],{\"abc\":123},[1.0,[[true],null]]]", //
                         ((struct TestcaseNode[]){
                             MAKE_ARRAY(3),
                             MAKE_ARRAY(2),
                             MAKE_OBJECT(0),
                             MAKE_ARRAY(0),
                             MAKE_OBJECT(2),
                             MAKE_STRING("abc"),
                             MAKE_INTEGER(123),
                             MAKE_ARRAY(2),
                             MAKE_REAL(1.0),
                             MAKE_ARRAY(2),
                             MAKE_ARRAY(1),
                             MAKE_BOOLEAN(1),
                             MAKE_NULL(),
                         }));
}

static void testcaseNodeSizes(void)
{
    testcaseInit("node_sizes");
    static const char kText[] = "[[],{},\"\",\"abc\",\"abcdefg\",0,-12345,12345,"
                                "0.0,-1.0,1.0,true,null,null]";
    struct JsonParser parser;
    testParserInit(&parser);
    JsonDocument *doc = jsonParse(kText, SIZEOF(kText) - 1, &parser);
    CHECK(doc);

    static const char *kNames[] = {
        "root header ", "empty object", "empty array ", "empty string", "string (3)  ",
        "string (7)  ", "integer (0) ", "integer (-) ", "integer (+) ", "real (0)    ",
        "real (+)    ", "real (-)    ", "boolean     ", "null        ",
    };
    char *values[COUNTOF(kNames) + 1];
    struct JsonCursor c;
    jsonCursorInit(jsonRoot(doc), kCursorRecursive, &c);
    for (JsonSize i = 0; jsonCursorIsValid(&c); ++i) {
        values[i] = (char *)jsonCursorValue(&c);
        jsonCursorNext(&c);
    }
    for (JsonSize i = 0; i < COUNTOF(kNames); ++i) {
        const JsonSize sizeInBytes = values[i + 1] - values[i];
        printf("    %s  %" PRId64 "\n", kNames[i], (int64_t)sizeInBytes);
    }
    jsonDestroyDocument(doc);
}

static void testcaseIntegers(void)
{
    testcaseInit("numbers_integer");
    ASSERT_JSON_EQUALS_R( //
        "[-9223372036854775808,"
        " -42389015228914,"
        " -24381906,"
        " -2394,"
        " -1,"
        " 0,"
        " 1,"
        " 3058,"
        " 39052783,"
        " 9082348975302,"
        " 9223372036854775807]",
        ((struct TestcaseNode[]){
            MAKE_ARRAY(11),
            MAKE_INTEGER(INT64_MIN),
            MAKE_INTEGER(-42389015228914),
            MAKE_INTEGER(-24381906),
            MAKE_INTEGER(-2394),
            MAKE_INTEGER(-1),
            MAKE_INTEGER(0),
            MAKE_INTEGER(1),
            MAKE_INTEGER(3058),
            MAKE_INTEGER(39052783),
            MAKE_INTEGER(9082348975302),
            MAKE_INTEGER(INT64_MAX),
        }));
}

static void testcaseReals(void)
{
    testcaseInit("numbers_real");
    ASSERT_JSON_EQUALS_R( //
        "[1.0,"
        " 1.000000000000000005,"
        " 1e-1000,"
        " 1e3,"
        " 1e+3,"
        " 1e-3,"
        " 1.2e3,"
        " 1.2e+3,"
        " 1.2e-3,"
        // Overflowing integers are parsed as real numbers.
        " 99999999999999999999,"
        " -9223372036854775809,"
        " 9223372036854775808,"
        " -9223372036854775809.5,"
        " 9223372036854775808.5]",
        ((struct TestcaseNode[]){
            MAKE_ARRAY(14),
            MAKE_REAL(1.0),
            MAKE_REAL(1.000000000000000005),
            MAKE_REAL(0.0),
            MAKE_REAL(1e3),
            MAKE_REAL(1e+3),
            MAKE_REAL(1e-3),
            MAKE_REAL(1.2e3),
            MAKE_REAL(1.2e+3),
            MAKE_REAL(1.2e-3),
            MAKE_REAL(99999999999999999999.0),
            MAKE_REAL((double)INT64_MIN - 1.0),
            MAKE_REAL((double)INT64_MAX + 1.0),
            MAKE_REAL((double)INT64_MIN - 1.5),
            MAKE_REAL((double)INT64_MAX + 1.5),
        }));
}

static void testcaseLiterals(void)
{
    testcaseInit("literals");
    ASSERT_JSON_EQUALS_R("[null,false,true]", //
                         ((struct TestcaseNode[]){
                             MAKE_ARRAY(3),
                             MAKE_NULL(),
                             MAKE_BOOLEAN(0),
                             MAKE_BOOLEAN(1),
                         }));
}

static void testCheckUtf8(const char *input, JsonBool expectOk)
{
    struct JsonParser parser;
    testParserInit(&parser);
    JsonDocument *doc = jsonParse(input, (JsonSize)strlen(input), &parser);
    if (expectOk) {
        CHECK(parser.status == kStatusOk);
        jsonDestroyDocument(doc);
    } else {
        CHECK(parser.status == kStatusStringInvalidUtf8);
    }
}

static void testcaseUtf8(void)
{
    testcaseInit("utf8");
    static const struct {
        const char *text;
        JsonBool isValid;
    } kUtf8[] = {
        // From @golang/go
        {"\"\xF4\x8F\xBF\xBF\"", 1}, // U+10FFFF
        {"\"\"", 1},
        {"\"a\"", 1},
        {"\"abc\"", 1},
        {"\"Ж\"", 1},
        {"\"ЖЖ\"", 1},
        {"\"брэд-ЛГТМ\"", 1},
        {"\"☺☻☹\"", 1},
        {"\"\"", 1},
        {"\"abcd\"", 1},
        {"\"☺☻☹\"", 1},
        {"\"日a本b語ç日ð本Ê語þ日¥本¼語i日©\"", 1},
        {"\"日a本b語ç日ð本Ê語þ日¥本¼語i日©日a本b語ç日ð本Ê語þ日¥本¼語i日©日a本b語ç日ð本Ê語þ日¥本¼語i"
         "日©\"",
         1},
        {"\"abcdefg abcdefg abcdefg abcdefg \"", 1},
        {"\"abcdefg abcdefg abcdefg abcdefg abcdefghabcdefghabcdefghabcdefgh\"", 1},

        // From @rusticstuff/simdutf8
        {"\"öäüÖÄÜß\"", 1},          // umlauts
        {"\"❤️✨🥺🔥😂😊✔️👍🥰\"", 1}, // emojis
        {"\"断用山昨屈内銀代意検瓶調像。情旗最投任留財夜隆年表高学送意功者。辺図掲記込真通第民国聞"
         "平。海帰傷芸記築世防橋整済歳権君注。選紙例並情夕破勢景移情誇進場豊読。景関有権米武野範随"
         "惑旬特覧刊野。相毎加共情面教地作減関絡。暖料児違歩致本感閉浦出楽赤何。時選権週邑針格事提"
         "一案質名投百定。止感右聞食三年外積文載者別。\"",
         1}, // Chinese
        {"\"意ざど禁23費サヒ車園オスミト規更ワエ異67事続トソキ音合岡治こ訪京ぴ日9稿がト明安イ抗的ウ"
         "クロコ売一エコヨホ必噴塗ッ。索墓ー足議需レ応予ニ質県トぴン学市機だほせフ車捕コニ自校がこ"
         "で極3力イい増娘汁表製ク。委セヤホネ作誌ミマクソ続新ほし月中報制どてびフ字78完りっせが村惹"
         "ヨサコ訳器りそ参受草ムタ大移ッけでつ番足ほこン質北ぽのよう応一ア輝労イ手人う再茨夕へしう"
         "。\"",
         1}, // Japanese
        {"\"3인은 대법원장이 지명하는 자를 임명한다, 대통령은 제3항과 제4항의 사유를 지체없이 "
         "공포하여야 한다, 제한하는 경우에도 자유와 권리의 본질적인 내용을 침해할 수 없다, 국가는 "
         "전통문화의 계승·발전과 민족문화의 창달에 노력하여야 한다.\"",
         1}, // Korean

        // From @golang/go
        {"\"\x80\x80\x80\x80\"", 0},
        {"\"\xed\xa0\x80\x80\"", 0}, // surrogate min
        {"\"\xed\xbf\xbf\x80\"", 0}, // surrogate max
        {"\"\x91\x80\x80\x80\"", 0}, // xx
        {"\"\xC2\x7F\x80\x80\"", 0}, // s1
        {"\"\xC2\xC0\x80\x80\"", 0},
        {"\"\xDF\x7F\x80\x80\"", 0},
        {"\"\xDF\xC0\x80\x80\"", 0},
        {"\"\xE0\x9F\xBF\x80\"", 0}, // s2
        {"\"\xE0\xA0\x7F\x80\"", 0},
        {"\"\xE0\xBF\xC0\x80\"", 0},
        {"\"\xE0\xC0\x80\x80\"", 0},
        {"\"\xE1\x7F\xBF\x80\"", 0}, // s3
        {"\"\xE1\x80\x7F\x80\"", 0},
        {"\"\xE1\xBF\xC0\x80\"", 0},
        {"\"\xE1\xC0\x80\x80\"", 0},
        {"\"\xED\x7F\xBF\x80\"", 0}, // s4
        {"\"\xED\x80\x7F\x80\"", 0},
        {"\"\xED\x9F\xC0\x80\"", 0},
        {"\"\xED\xA0\x80\x80\"", 0},
        {"\"\xF0\x8F\xBF\xBF\"", 0}, // s5
        {"\"\xF0\x90\x7F\xBF\"", 0},
        {"\"\xF0\x90\x80\x7F\"", 0},
        {"\"\xF0\xBF\xBF\xC0\"", 0},
        {"\"\xF0\xBF\xC0\x80\"", 0},
        {"\"\xF0\xC0\x80\x80\"", 0},
        {"\"\xF1\x7F\xBF\xBF\"", 0}, // s6
        {"\"\xF1\x80\x7F\xBF\"", 0},
        {"\"\xF1\x80\x80\x7F\"", 0},
        {"\"\xF1\xBF\xBF\xC0\"", 0},
        {"\"\xF1\xBF\xC0\x80\"", 0},
        {"\"\xF1\xC0\x80\x80\"", 0},
        {"\"\xF4\x7F\xBF\xBF\"", 0}, // s7
        {"\"\xF4\x80\x7F\xBF\"", 0},
        {"\"\xF4\x80\x80\x7F\"", 0},
        {"\"\xF4\x8F\xBF\xC0\"", 0},
        {"\"\xF4\x8F\xC0\x80\"", 0},
        {"\"\xF4\x90\x80\x80\"", 0},     // U+10FFFF+1; out of range
        {"\"\xF7\xBF\xBF\xBF\"", 0},     // 0x1FFFFF; out of range
        {"\"\xFB\xBF\xBF\xBF\xBF\"", 0}, // 0x3FFFFFF; out of range
        {"\"\xc0\x80\"", 0},             // U+0000 encoded in two bytes: incorrect
        {"\"\xed\xa0\x80\"", 0},         // U+D800 high surrogate (sic)
        {"\"\xed\xbf\xbf\"", 0},         // U+DFFF low surrogate (sic)
        {"\"aa\xe2\"", 0},
    };
    char buffer[1024];
    for (JsonSize i = 0; i < COUNTOF(kUtf8); ++i) {
        strcpy(buffer, kUtf8[i].text);
        testCheckUtf8(buffer, kUtf8[i].isValid);
    }
}

// static JsonBool multiDocEndContainer(void *ctx, JsonBool isObject)
//{
//     testEndContainer(ctx, isObject);
//     return 0; // Stop on the first ']' or '}'
// }

// static void testcaseMultipleDocs(void)
//{
//     testcaseInit("multiple_docs");
//     static const int kExpectedTypes[] = {
//         kTypeObjectBegin, kTypeObjectEnd,   kTypeArrayBegin, kTypeArrayEnd,   kTypeObjectBegin,
//         kTypeString,      kTypeInteger,     kTypeObjectEnd,  kTypeArrayBegin, kTypeBoolean,
//         kTypeArrayEnd,    kTypeObjectBegin, kTypeObjectEnd,  kTypeArrayBegin, kTypeInteger,
//         kTypeInteger,     kTypeInteger,     kTypeArrayEnd,   kTypeArrayBegin, kTypeArrayEnd,
//         kTypeObjectBegin, kTypeString,      kTypeInteger,    kTypeObjectEnd,
//     };
//     static const char kInput[] = "{}[]{\"a\":1} [true]\n{}\r\n[1,2,3]     []\t{\"b\":1}";
//     JsonSize length = SIZEOF(kInput) - 1;
//     struct JsonParser parser;
//     struct TestcaseContext ctx;
//     testParserInit(&parser, &ctx);
//     parser.h.endContainer = multiDocEndContainer;
//
//     const char *ptr = kInput;
//     while (length > 0) {
//         enum JsonStatus s = jsonParse(ptr, length, &parser);
//         CHECK(s == kStatusStopped);
//         length -= parser.offset;
//         ptr += parser.offset;
//     }
//     for (JsonSize i = 0; i < ctx.length; ++i) {
//         CHECK(ctx.nodes[i].type == kExpectedTypes[i]);
//     }
// }

static void testcaseLonelyValues(void)
{
    testcaseInit("lonely_values");
    ASSERT_JSON_EQUALS_R("{}", ((struct TestcaseNode[]){MAKE_OBJECT(0)}));
    ASSERT_JSON_EQUALS_R("[]", ((struct TestcaseNode[]){MAKE_ARRAY(0)}));
    ASSERT_JSON_EQUALS_R("\"string\"", ((struct TestcaseNode[]){MAKE_STRING("string")}));
    ASSERT_JSON_EQUALS_R("42", ((struct TestcaseNode[]){MAKE_INTEGER(42)}));
    ASSERT_JSON_EQUALS_R("1.5e-2", ((struct TestcaseNode[]){MAKE_REAL(1.5e-2)}));
    ASSERT_JSON_EQUALS_R("true", ((struct TestcaseNode[]){MAKE_BOOLEAN(1)}));
    ASSERT_JSON_EQUALS_R("false", ((struct TestcaseNode[]){MAKE_BOOLEAN(0)}));
    ASSERT_JSON_EQUALS_R("null", ((struct TestcaseNode[]){MAKE_NULL()}));
}

static void testcaseNormalCursor(void)
{
    testcaseInit("normal_cursor");
    ASSERT_JSON_EQUALS_N("[1,2,3]", //
                         ((struct TestcaseNode[]){
                             MAKE_INTEGER(1),
                             MAKE_INTEGER(2),
                             MAKE_INTEGER(3),
                         }));
    ASSERT_JSON_EQUALS_N("[1,[2,[3],4],5]", //
                         ((struct TestcaseNode[]){
                             MAKE_INTEGER(1),
                             MAKE_ARRAY(3),
                             MAKE_INTEGER(5),
                         }));
    ASSERT_JSON_EQUALS_N("{\"x\":null,\"y\":[[[1]]]}", //
                         ((struct TestcaseNode[]){
                             MAKE_KEY("x"),
                             MAKE_NULL(),
                             MAKE_KEY("y"),
                             MAKE_ARRAY(1),
                         }));
    ASSERT_JSON_EQUALS_N(
        "{\"a\":[1,2,3],\"b\":{\"x\":null,\"y\":[[[1]]]},\"c\":[1,[2,[3],4],5]}", //
        ((struct TestcaseNode[]){
            MAKE_KEY("a"),
            MAKE_ARRAY(3),
            MAKE_KEY("b"),
            MAKE_OBJECT(4),
            MAKE_KEY("c"),
            MAKE_ARRAY(3),
        }));
}

static void testcaseEmbeddedNull(void)
{
    testcaseInit("embedded_null");
    ASSERT_INVALID_JSON("123\0");
    ASSERT_INVALID_JSON("1.0\0");
    ASSERT_INVALID_JSON("false\0");
    ASSERT_INVALID_JSON("null\0");
    ASSERT_INVALID_JSON("\"abc\"\0");
    ASSERT_INVALID_JSON("[1,2,3]\0");
    ASSERT_INVALID_JSON("{\"k\":123}\0");
}

int main(void)
{
    puts("* * * running mewjson tests * * *");

    sanityCheckRealEquality();
    runExternalPassFailTests();
    runInternalPassFailTests();
    testcaseExpectedTypes();
    testcaseNodeSizes();
    testcaseOom();
    testcaseExceedMaxDepth();
    testcaseLargeNumbers();
    testcaseParseErrors();
    testcaseLiterals();
    testcaseUtf8();
    testcaseReals();
    testcaseIntegers();
    testcaseLonelyValues();
    testcaseNormalCursor();
    //    testcaseMultipleDocs();
    testcaseEmbeddedNull();
    checkForLeaks();

    puts("* * * passed all testcases * * *");
    return 0;
}
