add_library(lexer lexer.hpp lexer.cpp)
add_library(AST AST.hpp AST.cpp)
add_library(parser parser.hpp parser.cpp)
add_library(code_generator code_generator.hpp code_generator.cpp)

target_link_libraries(lexer
  PRIVATE fmt::fmt
  "$<$<CONFIG:DEBUG>:-fsanitize=address>"
)

target_link_libraries(AST
  PRIVATE fmt::fmt
  PRIVATE lexer
  "$<$<CONFIG:DEBUG>:-fsanitize=address>"
)

target_link_libraries(parser
  PRIVATE fmt::fmt
  PRIVATE lexer
  PRIVATE AST
  "$<$<CONFIG:DEBUG>:-fsanitize=address>"
)

target_link_libraries(code_generator
  PRIVATE fmt::fmt
  PRIVATE ${llvm_libs}
  PRIVATE AST
  "$<$<CONFIG:DEBUG>:-fsanitize=address>"
)
