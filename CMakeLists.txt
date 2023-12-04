cmake_minimum_required(VERSION 3.14)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
project(mewjson
        LANGUAGES C
        VERSION 0.0.1)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED On)
set(CMAKE_C_EXTENSIONS Off)

set(MAIN_PROJECT Off)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(MAIN_PROJECT On)
endif()

option(MEWJSON_BuildFuzzers "Build the fuzz targets" Off)
option(MEWJSON_BuildTests "Build the tests" ${MAIN_PROJECT})

add_library(mewjson_warnings INTERFACE)
if(${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(mewjson_warnings
            INTERFACE /WX /W3)
else()
    target_compile_options(mewjson_warnings
            INTERFACE -Werror -Wall -Wextra -Wpedantic
                      -Wsign-conversion -Wformat=2)
endif()

add_library(mewjson STATIC)
target_sources(mewjson
        PRIVATE mewjson.c
        PUBLIC mewjson.h)
target_include_directories(mewjson
        PUBLIC ${PROJECT_SOURCE_DIR})
target_link_libraries(mewjson
        PRIVATE mewjson_warnings)

if(MEWJSON_BuildFuzzers)
    add_subdirectory(fuzz)
endif()

if(MEWJSON_BuildTests)
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()
