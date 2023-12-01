
add_executable(test_parser test.c)
target_link_libraries(test_parser
        PRIVATE mewjson mewjson_warnings)
target_compile_definitions(test_parser
        PRIVATE TESTCASE_PREFIX="${CMAKE_CURRENT_SOURCE_DIR}/testcases/")
add_test(NAME test_parser
        COMMAND test_parser)
