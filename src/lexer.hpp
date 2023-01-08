#pragma once

#include <variant>
#include <string>
#include <fmt/core.h>
#include <unordered_map>
#include <vector>
#include <ranges>
#include <string_view>

enum TokenType {
  TOK_LEFT_PAREN,
  TOK_RIGHT_PAREN,
  TOK_LEFT_BRACE,
  TOK_RIGHT_BRACE,
  TOK_LEFT_CURLY,
  TOK_RIGHT_CURLY,
  TOK_MINUS,
  TOK_PLUS,
  TOK_SLASH,
  TOK_STAR,
  TOK_SEMICOLON,
  TOK_COMMA,
  TOK_DOT,
  TOK_BANG,
  TOK_BANG_EQUAL,
  TOK_EQUAL,
  TOK_EQUAL_EQUAL,
  TOK_GREATER,
  TOK_GREATER_EQUAL,
  TOK_LESS,
  TOK_LESS_EQUAL,
  TOK_IDENTIFIER,
  TOK_STRING,
  TOK_NUMBER,
  TOK_BOOL,
  TOK_AND,
  TOK_ELSE,
  TOK_FALSE,
  TOK_FN,
  TOK_FOR,
  TOK_IF,
  TOK_NULL,
  TOK_OR,
  TOK_RETURN,
  TOK_TRUE,
  TOK_VAR,
  TOK_WHILE,
  TOK_EOF,
  TOK_FAIL
};

// Possible types of data:
// string, double, bool, null

typedef std::variant<std::string, double, bool> Literal;

class Token {
public:
  TokenType type;
  std::string lexeme;
  // If the token is a literal value (number or string)
  Literal literal;
  // Information about position in file
  size_t line;
  size_t col;

  Token(TokenType __type, const std::string& __lexeme,
        const Literal& __literal, size_t __line, size_t __col) :
  type(__type), lexeme(__lexeme), literal(__literal),
  line(__line), col(__col) { }
};

class lexer_exception : std::exception {
  std::string info;
public:
  lexer_exception(size_t cur_col, size_t cur_line, const std::string& in)
  : info(fmt::format("(line {}, col {}) : {}", cur_line, cur_col, in)) { }
  virtual const char* what() {
    return info.c_str();
  }

};

class Lexer {
  inline static const std::unordered_map<std::string, TokenType> keyword_lookup {
      {"and", TOK_AND},
      {"else", TOK_ELSE},
      {"false", TOK_FALSE},
      {"fn", TOK_FN},
      {"for", TOK_FOR},
      {"if", TOK_IF},
      {"null", TOK_NULL},
      {"or", TOK_OR},
      {"return", TOK_RETURN},
      {"true", TOK_TRUE},
      {"var", TOK_VAR},
      {"while", TOK_WHILE},
  };

  std::vector<Token> tokens{};
  std::string source{};

  // Lexeme info
  size_t start_ind{};
  size_t current_ind{};

  // Source file info
  size_t cur_col{};
  size_t cur_line{1};

  static bool isalphanumeric(char c) {
    return std::isalpha(c) || std::isdigit(c);
  }

  void move_cursor(size_t to_move = 1) {
    current_ind += to_move;
    cur_col += to_move;
  }

  void throw_exception(const std::string& info) {
    add_token(TOK_FAIL);
    throw lexer_exception(cur_col,cur_line, info);
  }

  void new_line() {
    cur_col = 0;
    ++cur_line;
  }

  [[nodiscard]]
  std::string get_cur_lexeme(size_t offset = 0) {
    return source.substr(start_ind, current_ind - start_ind + offset);
  }

  void advance_lexeme() {
    if (start_ind == current_ind)
      move_cursor();
    start_ind = current_ind;
  }

  template <typename Func>
  void move_while(Func&& predicate) {
    while (predicate(peek()) && !is_end())
      move_cursor();
  }

  [[nodiscard]]
  char peek_next() {
    if (current_ind + 1 >= source.size())
      return '\0';
    return source.at(current_ind + 1);
  }

  bool match(char c) {
    if (peek_next() == c) {
      move_cursor(2);
      return true;
    }
    return false;
  }

  [[nodiscard]]
  char peek() {
    if (is_end())
      return '\0';
    return source.at(current_ind);
  }

  [[nodiscard]]
  bool is_end() { return current_ind >= source.size(); }

  void add_token(TokenType type, const std::string &lexeme = "",
                 const Literal &literal = Literal()) {
    tokens.emplace_back(type, lexeme, literal, cur_line, current_ind);
  }
  
  void handle_number();
  void handle_string();
  void handle_identifier();

  void scan_token();

public:
  [[nodiscard]]
  const std::vector<Token>& get_tokens(const std::string&);
};
