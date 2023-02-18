#include <fmt/core.h>
#include <iostream>
#include <fstream>
#include <sstream>

// #define DEBUG

#include "lexer.hpp"
#include "parser.hpp"

enum Return_values : int {
  SUCCESS = 0,
  LEXER_ERROR = 1,
  PARSER_ERROR = 2,
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

// void repl() {
//   std::string line;
//   fmt::print("> ");
//   Lexer lexer{};
//   Parser parser{};
//   while (std::getline(std::cin, line)) {
//     try {
//       auto tokens = lexer.get_tokens(line);
//       // fmt::print("{}\n", fmt::join(tokens, "\n"));
//       fmt::print("[LEXER PASS  ]\n");
//       auto ast = parser.parse(tokens);
//       fmt::print(ast -> pretty_show());
//       fmt::print("[PARSER PASS ]\n");
//     } catch (lexer_exception& e) {
//       fmt::print("[LEXER ERROR ]  {}\n", e.what());
//     } catch (parser_exception& e) {
//       fmt::print("[PARSER ERROR]  {}\n", e.what());
//     }
//     fmt::print("> ");
//   }
// }

std::string slurp(std::string file) {
  std::ifstream input(file);
  std::stringstream ss;
  ss << input.rdbuf();
  return ss.str();
}

void file_read(std::string file) {
  std::string input = slurp(file);
  Lexer lexer{};
  Parser parser{};
  try {
    auto tokens = lexer.get_tokens(input);
    // fmt::print("{}\n", fmt::join(tokens, "\n")); // DEBUG
    fmt::print("[LEXER PASS]\n");
    auto ast = parser.parse(tokens);
    // fmt::print("\n{}\n", ast -> pretty_show());
    fmt::print("[PARSER PASS]\n");
  } catch (lexer_exception& e) {
    fmt::print("[LEXER ERROR]  {}\n", e.what());
    exit(LEXER_ERROR);
  } catch (parser_exception& e) {
    fmt::print("[PARSER ERROR]  {}\n", e.what());
    exit(PARSER_ERROR);
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
