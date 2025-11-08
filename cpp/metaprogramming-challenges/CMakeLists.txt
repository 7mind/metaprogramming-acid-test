cmake_minimum_required(VERSION 3.20)
project(MetaprogrammingChallenges CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler warnings
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Enable RTTI (needed for Challenge 2 subtyping)
if(MSVC)
    add_compile_options(/GR)
else()
    add_compile_options(-frtti)
endif()

# Library: All challenges
add_library(metaprogramming_challenges INTERFACE)
target_include_directories(metaprogramming_challenges INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

# Example executables
add_executable(example_structured_logging examples/structured_logging_example.cpp)
target_link_libraries(example_structured_logging PRIVATE metaprogramming_challenges)

add_executable(example_type_reflection examples/type_reflection_example.cpp)
target_link_libraries(example_type_reflection PRIVATE metaprogramming_challenges)

add_executable(example_functoid examples/functoid_example.cpp)
target_link_libraries(example_functoid PRIVATE metaprogramming_challenges)

add_executable(example_all examples/all_examples.cpp)
target_link_libraries(example_all PRIVATE metaprogramming_challenges)

# Tests
enable_testing()

add_executable(test_structured_logging tests/test_structured_logging.cpp)
target_link_libraries(test_structured_logging PRIVATE metaprogramming_challenges)
add_test(NAME StructuredLogging COMMAND test_structured_logging)

add_executable(test_type_reflection tests/test_type_reflection.cpp)
target_link_libraries(test_type_reflection PRIVATE metaprogramming_challenges)
add_test(NAME TypeReflection COMMAND test_type_reflection)

add_executable(test_functoid tests/test_functoid.cpp)
target_link_libraries(test_functoid PRIVATE metaprogramming_challenges)
add_test(NAME Functoid COMMAND test_functoid)
