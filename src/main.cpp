#include <fmt/core.h>
#include <fmt/ostream.h>
#include <iostream>
#include "lexer.hpp"

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

void repl() {
  std::string line;
  fmt::print("> ");
  Lexer lexer{};
  while (std::getline(std::cin, line)) {
    try {
      auto tokens = lexer.get_tokens(line);
      fmt::print("{}\n", fmt::join(tokens, "\n"));
    } catch (lexer_exception& e) {
      fmt::print("[LEXER ERROR]   {}\n", e.what());
    }
    fmt::print("> ");
  }
}

int main(int argc, char* argv[]) {
  repl();
  return 0;
}
