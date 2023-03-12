#include <fmt/core.h>
#include <iostream>
#include <fstream>
#include <sstream>

// #define DEBUG

#include "lexer.hpp"
#include "parser.hpp"
#include "AST.hpp"
#include "code_generator.hpp"

enum Return_values : int {
  SUCCESS = 0,
  LEXER_ERROR = 1,
  PARSER_ERROR = 2,
  INTERPRETER_ERROR = 3,
  UNEXPECTED_ERROR = 4,
};

std::ostream& operator<<(std::ostream& os, const Token& t) {
  if (t.type == TOK_EOF)  os << "EOF";
  if (t.type == TOK_FAIL) os << "FAIL";
  else os << t.lexeme << " : " << t.type;
  if (t.type == TOK_NUMBER)
    os << " = " << std::get<double>(t.literal);
  if (t.type == TOK_BOOL)
    os << std::boolalpha << " = " << std::get<bool>(t.literal);
  if (t.type == TOK_STRING)
    os << " = " << std::get<std::string>(t.literal);
  return os;
}

template <> struct fmt::formatter<Token> : ostream_formatter {};

std::string slurp(std::string file) {
  std::ifstream input(file);
  std::stringstream ss;
  ss << input.rdbuf();
  return ss.str();
}

Return_values file_read(std::string file) {
  std::string input = slurp(file);
  try {
    Lexer lexer{};
    auto tokens = lexer.get_tokens(input);
    fmt::print(stderr, "[LEXER PASS]\n");

    Parser parser{};
    auto ast = parser.parse(tokens);
    fmt::print(stderr, "[PARSER PASS]\n");

    CodeGenerator cg{};

    cg(ast);
    
    fmt::print(stderr, "[INTERPRETER PASS]\n");

    llvm::outs() << *cg.the_module << '\n';

    return SUCCESS;
  } catch (lexer_exception& e) {
    fmt::print(stderr, "[LEXER ERROR]  {}\n", e.what());
    return LEXER_ERROR;
  } catch (parser_exception& e) {
    fmt::print(stderr, "[PARSER ERROR]  {}\n", e.what());
    return PARSER_ERROR;
  } 
  catch (interpreter_exception& e) {
    fmt::print(stderr, "[INTERPRETER ERROR]  {}\n", e.what());
    return INTERPRETER_ERROR;
  } 
  catch (std::exception& e) {
    fmt::print(stderr, "[UNEXPECTED ERROR]  {}\n", e.what());
    return UNEXPECTED_ERROR;
  }
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    return file_read(argv[1]);
  } else {
    return SUCCESS;
  }
}
