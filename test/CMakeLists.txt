
function(build_test NAME)
    add_executable(${NAME} ${NAME}.c test.c test.h)
    target_link_libraries(${NAME}
            PRIVATE mewjson)
    target_link_libraries(${NAME}
            PRIVATE mewjson)
    add_test(NAME ${NAME}
            COMMAND ${NAME})
endfunction()

build_test(test_read_write)
build_test(test_document)

# Read/write tests need to know where to look to find the testcase files.
target_compile_definitions(test_read_write
        PRIVATE TESTCASE_PREFIX="${CMAKE_CURRENT_SOURCE_DIR}/testcases/")