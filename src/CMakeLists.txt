add_library(lexer lexer.hpp lexer.cpp)
add_library(AST AST.hpp AST.cpp)
add_library(parser parser.hpp parser.cpp)

target_link_libraries(lexer
  PRIVATE fmt::fmt)

target_link_libraries(AST
  PRIVATE fmt::fmt
  PRIVATE lexer
  PRIVATE ${llvm_libs})

target_link_libraries(parser
  PRIVATE fmt::fmt
  PRIVATE lexer
  PRIVATE AST)
