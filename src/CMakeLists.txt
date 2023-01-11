add_library(lexer lexer.hpp lexer.cpp)
add_library(parser parser.hpp parser.cpp)

target_link_libraries(lexer
  PRIVATE fmt::fmt)

target_link_libraries(parser
  PRIVATE fmt::fmt
  PRIVATE lexer)
