
add_executable(fuzz_parser fuzz.c)
target_link_libraries(fuzz_parser
        PRIVATE mewjson mewjson_warnings)