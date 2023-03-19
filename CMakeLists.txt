cmake_minimum_required(VERSION 3.15.0)

enable_language(C)
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

llvm_map_components_to_libnames(llvm_libs support core irreader analysis target mc passes) 

# ------ kavun -------

project(kavun)

add_executable(${PROJECT_NAME} src/main.cpp)

add_subdirectory(src)
add_subdirectory(lib)

target_link_libraries(${PROJECT_NAME}
  PRIVATE lexer parser AST code_generator
  PRIVATE fmt::fmt
  PRIVATE ${llvm_libs}
  -fsanitize=address # Debug only
  )

enable_testing()

add_subdirectory(tests)
