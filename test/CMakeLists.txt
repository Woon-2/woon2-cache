enable_testing()

add_executable(woon2cache_test)

target_sources(woon2cache_test
PRIVATE
    "${PROJECT_SOURCE_DIR}/LRUCache.hpp"
    random_object.hpp
    test_values.hpp
    LRU_test.cpp
    rule_of_five_test.cpp
)

target_compile_features(woon2cache_test PRIVATE cxx_std_20)

if(CMAKE_CXX_COMPILER_ID MATCHES MSVC)
    target_compile_options(woon2cache_test
    PRIVATE
        /MP     # compilation speed-up via multi processor
        $<IF:$<CONFIG:DEBUG>,/MDd,/MD>    # use dynamic multi-threaded crt library
        /sdl    # security check
        $<IF:$<CONFIG:RELEASE>,/O2,/Od>     # maximum speed optimization for Release
        /fp:strict    # rate correctness over speed on floating points
        /Zc:preprocessor    # standard behavior of preprocessor
        /W3     # warning level
    )

elseif(CMAKE_CXX_COMPILER_ID MATCHES GNU)
    target_compile_options(woon2cache_test
    PRIVATE
        -fconcepts
        -Wall
        -pedantic
        -Wextra
    )

elseif(CMAKE_CXX_COMPILER_ID MATCHES Clang)     
     # Clang options differ by platform.
     if(WIN32)   # clang-cl
        target_compile_options(woon2cache_test
        PRIVATE
            clang:-fcoroutines-ts
            -fms-compatiability)
    else()  # AppleClang or Clang on Linux
     target_compile_options(woon2cache_test
     PRIVATE
         -std=c++2a
     )
    endif()
else()
    message(WARNING "configuring with unknown compiler...")
endif()

# Find and setup google test
message(STATUS "finding GTest...")

find_package(GTest)

if(Gtest_FOUND)
    message(STATUS "GTest - Found.")
else()
    message(STATUS "GTest - Not found.")
    message(STATUS "Downloading GTest from github...")

    set(GTEST_COMMIT_HASH "b10fad38c4026a29ea6561ab15fc4818170d1c10" CACHE
        STRING "set git commit hash for specific version of google test." FORCE)
    
    # for consistent behavior of FetchContent
    if (POLICY CMP0135)
        cmake_policy(SET CMP0135 OLD)
    endif()

    include(FetchContent)
    FetchContent_Declare(
        googletest
        URL "https://github.com/google/googletest/archive/${GTEST_COMMIT_HASH}.zip"
    )

    message(STATUS "GTest (${GTEST_COMMIT_HASH}) Downloaded.")

    # For Windows: Prevent overriding the parent project's compiler/linker settings
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

    FetchContent_MakeAvailable(googletest)
endif()

target_link_libraries(woon2cache_test PRIVATE GTest::gtest_main woon2cache)
# TODO: separate paths for build interface/install interface
target_include_directories(woon2cache_test PRIVATE "${PROJECT_SOURCE_DIR}" "${gtest_SOURCE_DIR}/include")

gtest_discover_tests(woon2cache_test)

# TODO: separate paths for build interface/install interface
add_custom_target(
    test
    COMMENT "test features"
    COMMAND ctest -C $<CONFIG> --test-dir "${PROJECT_BINARY_DIR}/test" --output-on-failure
)

option(BUILD_TESTING
    "enable if you want to test each time you build the project."
    OFF
)
if (BUILD_TESTING)
    add_custom_command(TARGET woon2cache_test POST_BUILD
        COMMAND "${CMAKE_COMMAND}" --build "${PROJECT_BINARY_DIR}" --target test
        COMMENT "Building test target after all woon2cache_test target is built."
    )
endif()