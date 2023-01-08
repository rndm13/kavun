add_library(lexer lexer.hpp lexer.cpp)

target_link_libraries(lexer
  PRIVATE fmt::fmt)
