#include "mewjson.h"
#include <float.h>
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

void checkForLeaks(void)
{
    CHECK(sBytesUsed == 0);
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

    // Overflowing integers become reals. Overflowing reals become Inf or -Inf and are
    // written as "null" by jsonWrite().
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
    // +/-Inf. jsonWrite() will write them as nulls.
    "fail60",
    "fail73",

    // TODO: These should fail due to bad UTF-8.
    "i_string_overlong_sequence_2_bytes",
    "i_string_not_in_unicode_range",
};

// clang-format off
static struct Testcase {
    const char *name;
    const char *input;
    JsonSize inputLen;
    enum TestcaseType {
        kTestcaseNo,
        kTestcaseYes,
        kTestcaseYesInteger,
        kTestcaseYesReal,
    } type;
    union TestcaseValue {
        int64_t integer;
        double real;
    } value;
} sInternalTestcases[] = {
#define YES(a, b) {.name = CAT(y_, a), .input = b, .inputLen = (JsonSize)strlen(b), .type = kTestcaseYes},
    YES(structure_complicated, "[{\"a\":[{}]},{\"b\":[{\"c\":[[],[],[true]],\"d\":null}]}]")
    YES(array_nested, "[1,[2,[3,4],5,[6,7],8],9,[10,[11,12],13,[14,15],16],17]")

#define YES_INTEGER(a, b, c) {.name = CAT(y_, a), .input = b, .inputLen = (JsonSize)strlen(b), .type = kTestcaseYesInteger, .value = (union TestcaseValue){.integer = c}},
    YES_INTEGER(number_simple_int, "123", 123)
    YES_INTEGER(number_negative_int, "-123", -123)
    YES_INTEGER(number_after_space, " 4", 4)
    YES_INTEGER(number_negative_one, "-1", -1)
    YES_INTEGER(number_negative_zero, "-0", 0)
    YES_INTEGER(number_integer_min, "-9223372036854775808", INT64_MIN)
    YES_INTEGER(number_integer_max, "9223372036854775807", INT64_MAX)
    YES_INTEGER(number_integer_small, "-9223372036854775807", INT64_MIN + 1)

#define YES_REAL(a, b, c) {.name = CAT(y_, a), .input = b, .inputLen = (JsonSize)strlen(b), .type = kTestcaseYesReal, .value = (union TestcaseValue){.real = c}},
    YES_REAL(structure_lonely_negative_real, "-0.1", -0.1)
    YES_REAL(number_simple_real, "123.456789", 123.456789)
    YES_REAL(number_real_positive_exponent, "1e+2", 1e+2)
    YES_REAL(number_real_negative_exponent, "1e-2", 1e-2)
    YES_REAL(number_real_small, "-1.0e+28", -1.0e+28)
    YES_REAL(number_real_large_1, "1.0e+28", 1.0e+28)
    YES_REAL(number_real_large_2, "123e45", 123e45)
    YES_REAL(number_real_large_3, "123e65", 123e65)
    YES_REAL(number_real_close_to_zero,
             "-0.000000000000000000000000000000000000000000000000000000000000000000000000000001",
             -0.000000000000000000000000000000000000000000000000000000000000000000000000000001)
    YES_REAL(number_integer_with_exponent, "20e1", 20e1) // Becomes a real
    YES_REAL(number_real_capital_e_pos_exponent, "1E+2", 1E+2)
    YES_REAL(number_real_capital_e, "1E22", 1E22)
    YES_REAL(number_real_fraction_exponent, "123.456e78", 123.456e78)
    YES_REAL(number_real_capital_e_negative_exponent, "1E-2", 1E-2)
    YES_REAL(number_0e+1, "0e+1", 0e+1)
    YES_REAL(number_0e-1, "0e-1", 0e-1)
    YES_REAL(number_0e1, "0e1", 0e1)
    YES_REAL(number_underflowing_integer_becomes_real_1, "-9223372036854775809", -9223372036854775809.0)
    YES_REAL(number_underflowing_integer_becomes_real_2, "-92233720368547758080", -92233720368547758080.0)
    YES_REAL(number_overflowing_integer_becomes_real_1, "9223372036854775808", 9223372036854775808.0)
    YES_REAL(number_overflowing_integer_becomes_real_2, "9223372036854775809", 9223372036854775809.0)
    YES_REAL(number_overflowing_integer_becomes_real_3, "92233720368547758199", 92233720368547758199.0)
    YES_REAL(number_exponent_with_leading_zero, "1.23e01", 1.23e01)
    YES_REAL(number_exponent_with_leading_zero_and_sign, "1.23e+01", 1.23e+01)
    YES_REAL(number_tiny_negative_real, "-1.0e-123", -1.0e-123)
    YES_REAL(number_negative_zero, "-0.0", -0.0)

#define NO(a, b) {.name = CAT(n_, a), .input = b, .inputLen = (JsonSize)strlen(b), .type = kTestcaseNo},
    NO(string_missing_high_surrogate, "\"\\uDC00\"")
    NO(object_with_trailing_comment, "{\"a\":\"b\"}/*comment*/")
    NO(object_with_leading_comment, "/*comment*/{\"a\":\"b\"}")
    NO(number_very_long_with_junk_at_end, "123456789012345678901234567890.0e*")
    NO(string_backslash_00, "[\"\\0\"]")
    NO(structure_lonely_array_end, "]")
    NO(structure_lonely_object_end, "}")
    NO(structure_embedded_EOF, "[\xFF]")
    NO(structure_EOF_at_end, "[]\xFF")
    NO(structure_EOF_at_end2, "[]\xFF\xFF")
    NO(string_with_EOF, "\"\xFF\"")
};
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
    jsonParserInit(parser, NULL,
                   &(struct JsonAllocator){
                       testMalloc,
                       testRealloc,
                       testFree,
                   });
}

static void testcaseInit(const char *name)
{
    puts(name);
    sOomCounter = -1;
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
    testParserInit(&parser);

    int failures = 0;
    for (;; ++failures) {
        sOomCounter = failures;
        const enum JsonParseStatus s =
            jsonParse(kOomExample, (JsonSize)strlen(kOomExample), &parser);
        if (s == kParseOk) {
            break;
        }
        CHECK(s == kParseNoMemory);
    }
    // Will likely end up failing quite a bit more than 2 times. Just a sanity check.
    CHECK(failures > 2);
}

// static void attemptRoundtrip(struct JsonParser *parser, JsonDocument **doc)
//{
//     JsonValue *v1 = jsonRoot(*doc);
//
//     // Determine how many bytes are needed to store the serialized document and allocate a buffer
//     // to write it into.
//     const JsonSize n = jsonWrite(NULL, 0, v1);
//     CHECK(n > 0); // Valid document is never empty
//     char *result = leakCheckMalloc((size_t)n + 1);
//     CHECK(result);
//
//     // Write the JSON text.
//     CHECK(n == jsonWrite(result, n, v1));
//     jsonDestroyDocument(*doc);
//
//     // Parse the JSON we just wrote.
//     JsonDocument *doc2 = jsonParse(result, n, parser);
//     CHECK(doc2);
//
//     // Write the JSON we just parsed to a different buffer.
//     char *result2 = leakCheckMalloc((size_t)n + 1);
//     CHECK(result2);
//     CHECK(n == jsonWrite(result2, n, jsonRoot(doc2)));
//
//     // The JSON text we wrote should match exactly.
//     CHECK(0 == memcmp(result, result2, (size_t)n));
//
//     leakCheckFree(result);
//     leakCheckFree(result2);
//     *doc = doc2;
// }

static void testcaseRun(struct Testcase *tc)
{
    testcaseInit(tc->name);

    struct JsonParser parser;
    testParserInit(&parser);
    const enum JsonParseStatus s = jsonParse(tc->input, tc->inputLen, &parser);
    if (tc->type == kTestcaseNo) {
        // Must fail for a reason other than running out of memory. OOM conditions are tested
        // separately (see TestcaseReaderOom()).
        CHECK(s != kParseNoMemory);
        return;
    }
    CHECK(s == kParseOk);
    //    attemptRoundtrip(&parser, &doc);
    //    JsonValue *v = jsonRoot(doc);
    //    if (tc->type == kTestcaseYesInteger) {
    //        CHECK(jsonType(v) == kTypeInteger);
    //        CHECK(jsonInteger(v) == tc->value.integer);
    //    } else if (tc->type == kTestcaseYesReal) {
    //        CHECK(jsonType(v) == kTypeReal);
    //        CHECK(areRealsEqual(jsonReal(v), tc->value.real));
    //    }
}

static void testcaseLargeNumbers(void)
{
    testcaseInit("large_numbers");
    static const char kText[] = "[-1e1111,1e1111]";

    struct JsonParser parser;
    testParserInit(&parser);
    CHECK(kParseOk == jsonParse(kText, (JsonSize)strlen(kText), &parser));

    //    JsonValue *arr = jsonRoot(doc);
    //    JsonValue *a = jsonContainerGet(arr, 0);
    //    JsonValue *b = jsonContainerGet(arr, 1);
    //    CHECK(jsonType(a) == kTypeReal);
    //    CHECK(areRealsEqual(jsonReal(a), -INFINITY));
    //    CHECK(jsonType(b) == kTypeReal);
    //    CHECK(areRealsEqual(jsonReal(b), INFINITY));
}

static void testcaseExceedMaxDepth(void)
{
    testcaseInit("exceed_max_depth");
    const JsonSize kDepth = 100000;
    char *buffer = leakCheckMalloc(kDepth + 1);
    for (size_t depth = 0; depth < kDepth; ++depth) {
        buffer[depth] = '[';
    }
    buffer[kDepth] = '\0';

    struct JsonParser parser;
    testParserInit(&parser);
    CHECK(kParseExceededMaxDepth == jsonParse(buffer, (JsonSize)strlen(buffer), &parser));
    leakCheckFree(buffer);
}

// Make sure the parser stops at the end of the input buffer. For this test to work
// properly, we have to make sure that strtod() doesn't run past the section of
// buffer made available to jsonRead(). If the number is adjacent to the end of the
// buffer, we have to copy it to a separate buffer and write a '\0'.
static void testcaseDigitsPastEndOfInput(void)
{
    testcaseInit("digits_past_end_of_input");
    //    const double kResult[] = {
    //        0.2, 0.23, 0.234, 0.2345, 0.23456, 0.234567, 0.2345678, 0.23456789,
    //    };
    //    const char kText[] = "0.23456789";

    struct JsonParser parser;
    testParserInit(&parser);
    //    for (JsonSize i = 3; i < 10; ++i) {
    //        CHECK(kParseOk == jsonParse(kText, i, &parser));
    //        CHECK(areRealsEqual(jsonReal(jsonRoot(doc)), kResult[i - 3]));
    //    }
}

static void testcaseParseErrors(void)
{
    testcaseInit("parse_errors");

    static const struct {
        enum JsonParseStatus reason;
        const char *text;
        JsonBool zeroOffset;
    } kInputs[] = {
        // Skip kParseOk
        // Skip kParseNoMemory
        // Skip kParseExceededMaxDepth
        {.reason = kParseStringInvalidEscape, .text = "\"\\xFF\""},
        {.reason = kParseStringInvalidCodepoint, .text = "\"\\uXYZW\""},
        {.reason = kParseStringMissingLowSurrogate, .text = "\"\\uD800\\uD800\""},
        {.reason = kParseStringMissingHighSurrogate, .text = "\"\\uDC00\""},
        {.reason = kParseStringUnescapedControl, .text = "\"\x01\""},
        {.reason = kParseStringInvalidUtf8, .text = "\"\xED\xA0\x80\""},
        {.reason = kParseLiteralInvalid, .text = "tru"},
        {.reason = kParseNumberMissingExponent, .text = "1.23e"},
        {.reason = kParseNumberMissingFraction, .text = "1.e23"},
        {.reason = kParseNumberInvalid, .text = "0123"},
        {.reason = kParseContainerNotClosed, .text = "[{}"},
        {.reason = kParseContainerNotClosed, .text = "["},
        {.reason = kParseSyntaxError, .text = "[]123"},
        {.reason = kParseSyntaxError, .text = "", .zeroOffset = 1},
        {.reason = kParseSyntaxError, .text = " "},
    };

    struct JsonParser parser;
    testParserInit(&parser);
    for (JsonSize i = 0; i < COUNTOF(kInputs); ++i) {
        CHECK(kInputs[i].reason ==
              jsonParse(kInputs[i].text, (JsonSize)strlen(kInputs[i].text), &parser));
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

    char *text = leakCheckMalloc((size_t)fileSize + 1);
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
    leakCheckFree(text);
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

int main(void)
{
    puts("* * * running RFC 8259 conformance tests * * *");

    sanityCheckRealEquality();
    runExternalPassFailTests();
    runInternalPassFailTests();
    testcaseReaderOom();
    testcaseDigitsPastEndOfInput();
    testcaseExceedMaxDepth();
    testcaseLargeNumbers();
    testcaseParseErrors();

    checkForLeaks();

    puts("* * * passed all testcases * * *");
    return 0;
}
