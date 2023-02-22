cmake_minimum_required(VERSION 3.15.0)

enable_language(CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -Wall -pedantic -Wextra -Wnon-virtual-dtor -Wshadow -fno-omit-frame-pointer")

# ------- fmt --------

include(FetchContent)

FetchContent_Declare(fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG master
)

FetchContent_MakeAvailable(fmt)

# ------- llvm -------

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(llvm_libs support core irreader passes target MC mc) 

# ------ kavun -------

project(kavun)

add_executable(${PROJECT_NAME} src/main.cpp)
add_subdirectory(src)

target_link_libraries(${PROJECT_NAME}
  PRIVATE lexer parser AST
  PRIVATE fmt::fmt
  PRIVATE ${llvm_libs}
  -fsanitize=address # Debug only
  )

# ------ tests -------

include(CTest)

set (lexer_error_regex "[LEXER ERROR]")
set (lexer_pass_regex  "[LEXER PASS]")

add_test(NAME lexer_pass1 COMMAND kavun tests/valid_program1.kvn)
set_property(TEST lexer_pass1 PROPERTY PASS_REGULAR_EXPRESSION "${lexer_pass_regex}")

add_test(NAME no_end_of_string COMMAND kavun tests/lexer_fail_no_end_string.kvn)
set_property(TEST no_end_of_string PROPERTY PASS_REGULAR_EXPRESSION "${lexer_error_regex}")

add_test(NAME double_floating_points COMMAND kavun tests/lexer_fail_2_points.kvn)
set_property(TEST double_floating_points PROPERTY PASS_REGULAR_EXPRESSION "${lexer_error_regex}")

add_test(NAME invalid_character COMMAND kavun tests/lexer_fail_inv_char.kvn)
set_property(TEST invalid_character PROPERTY PASS_REGULAR_EXPRESSION "${lexer_error_regex}")
