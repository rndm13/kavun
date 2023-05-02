/// \file

#pragma once

#include <variant>
#include <string>
#include <fmt/core.h>
#include <fmt/ostream.h>
#include <unordered_map>
#include <vector>
#include <ranges>
#include <string_view>
#include <functional>
#include <numeric>

#include <fmt/xchar.h>

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
  TOK_MODULO,
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
  TOK_BREAK,
  TOK_CONTINUE,
  TOK_NUMBER,
  TOK_BOOL,
  TOK_AND,
  TOK_ELSE,
  TOK_FALSE,
  TOK_FN,
  TOK_FOR,
  TOK_IF,
  TOK_OR,
  TOK_RETURN,
  TOK_TRUE,
  TOK_VAR,
  TOK_MODULE,
  TOK_EXTERN,
  TOK_EOF,
  TOK_FAIL,
};

// Possible types of data:
// string, double, bool, null

typedef std::variant<std::wstring, double, bool, std::uint32_t> Literal;

class Token {
public:
  TokenType type;
  std::wstring lexeme;
  // If the token is a literal value (number or string)
  Literal literal;
  // Information about position in file
  size_t line;
  size_t col;

  Token(TokenType __type, const std::wstring& __lexeme,
        const Literal& __literal, size_t __line, size_t __col) :
  type(__type), lexeme(__lexeme), literal(__literal),
  line(__line), col(__col) { }
};

class lexer_exception : std::exception {
  std::wstring info;
public:
  lexer_exception(size_t cur_col, size_t cur_line, const std::wstring& in)
  : info(fmt::format(L"(line {}, col {}) : {}", cur_line, cur_col, in)) { }
  virtual const wchar_t* what() {
    return info.c_str();
  }
};

class Lexer {
  inline static const std::unordered_map<std::wstring, TokenType> keyword_lookup {
      {L"and", TOK_AND},
      {L"else", TOK_ELSE},
      {L"false", TOK_FALSE},
      {L"fn", TOK_FN},
      {L"for", TOK_FOR},
      {L"if", TOK_IF},
      {L"or", TOK_OR},
      {L"return", TOK_RETURN},
      {L"true", TOK_TRUE},
      {L"var", TOK_VAR},
      {L"module", TOK_MODULE},
      {L"break", TOK_BREAK},
      {L"continue", TOK_CONTINUE},
      {L"extern", TOK_EXTERN},
// Ukrainian
      {L"та", TOK_AND},
      {L"інакше", TOK_ELSE},
      {L"хиба", TOK_FALSE},
      {L"фн", TOK_FN},
      {L"для", TOK_FOR},
      {L"якщо", TOK_IF},
      {L"або", TOK_OR},
      {L"повернути", TOK_RETURN},
      {L"істина", TOK_TRUE},
      {L"змін", TOK_VAR},
      {L"модуль", TOK_MODULE},
      {L"вихід", TOK_BREAK},
      {L"продовження", TOK_CONTINUE},
      {L"зовні", TOK_EXTERN},
  };

  std::vector<Token> tokens{};
  std::wstring source{};

  // Lexeme info
  size_t start_ind{};
  size_t current_ind{};

  // Source file info
  size_t cur_col{};
  size_t cur_line{1};

  static bool isalphanumeric(wchar_t c) {
    return ischaracter(c) || std::isdigit(c) || c == '_';
  }

  static bool ischaracter(wchar_t c) {
    return std::isalpha(c) || isukrainian(c);
  }

  static bool isukrainian(wchar_t c) {
    return 
      c == L'є' || c == L'Є' ||
      c == L'І' || c == L'Ї' ||
      (c >= L'А' && c <= L'я') ||
      c == L'є' || c == L'і' ||
      c == L'ї' || c == L'ґ' || 
      c == L'Ґ' || c == L'\'';
  }
    

  void move_cursor(size_t to_move = 1) {
    current_ind += to_move;
    cur_col += to_move;
  }

  void throw_exception(const std::wstring& info) {
    add_token(TOK_FAIL);
    throw lexer_exception(cur_col, cur_line, info);
  }

  void new_line() {
    cur_col = 0;
    ++cur_line;
  }

  [[nodiscard]]
  std::wstring get_cur_lexeme(size_t offset = 0) {
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
  wchar_t peek_next() {
    if (current_ind + 1 >= source.size())
      return '\0';
    return source.at(current_ind + 1);
  }

  [[nodiscard]]
  bool match(wchar_t c) {
    if (peek_next() == c) {
      move_cursor(2);
      return true;
    }
    return false;
  }

  [[nodiscard]]
  wchar_t peek() {
    if (is_end())
      return '\0';
    return source.at(current_ind);
  }

  [[nodiscard]]
  bool is_end() { return current_ind >= source.size(); }

  [[nodiscard]]
  std::wstring handle_escape_chars(const std::wstring&);

  void add_token(TokenType type, const std::wstring &lexeme = L"",
                 const Literal &literal = Literal{}) {
    tokens.emplace_back(type, lexeme, literal, cur_line, cur_col);
  }
  
  void handle_number();
  void handle_string();
  void handle_identifier();

  void scan_token();

public:
  [[nodiscard]]
  const std::vector<Token>& get_tokens(const std::wstring&);
};
