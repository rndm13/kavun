#include <fmt/core.h>
#include <iostream>
#include <fstream>
#include <sstream>

// #define DEBUG

#include "lexer.hpp"
#include "parser.hpp"
#include "AST.hpp"

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

void file_read(std::string file) {
  std::string input = slurp(file);
  try {
    Lexer lexer{};
    auto tokens = lexer.get_tokens(input);
    fmt::print("[LEXER PASS]\n");

    Parser parser{};
    auto ast = parser.parse(tokens);
    fmt::print("[PARSER PASS]\n");

    Interpreter interp(std::forward<ModuleAST::Ptr>(ast));
    auto result = interp.run();
    fmt::print("[INTERPRETER PASS]\n");
    llvm::errs() << *result << '\n';
  } catch (lexer_exception& e) {
    fmt::print("[LEXER ERROR]  {}\n", e.what());
    exit(LEXER_ERROR);
  } catch (parser_exception& e) {
    fmt::print("[PARSER ERROR]  {}\n", e.what());
    exit(PARSER_ERROR);
  } catch (interpreter_exception& e) {
    fmt::print("[INTERPRETER ERROR]  {}\n", e.what());
    exit(INTERPRETER_ERROR);
  } catch (std::exception& e) {
    fmt::print("[UNEXPECTED ERROR]  {}\n", e.what());
    exit(UNEXPECTED_ERROR);
  }
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    file_read(argv[1]);
  } else {
    // repl or usage
  }
  return SUCCESS;
}
