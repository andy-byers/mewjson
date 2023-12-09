
add_executable(fuzz fuzz.c)
target_link_libraries(fuzz
        PRIVATE mewjson mewjson_context)
