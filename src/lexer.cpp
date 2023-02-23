#include "lexer.hpp"

[[nodiscard]]
const std::vector<Token>& Lexer::get_tokens(const std::string& str) {
  source = str;
  start_ind = 0;
  current_ind = 0;
  cur_col = 0;
  cur_line = 1;

  while(!is_end())
    scan_token();
  add_token(TOK_EOF);
  return tokens;
}

void Lexer::scan_token() {
  char c = peek();
  switch (c) {
  break; case '(': add_token(TOK_LEFT_PAREN, get_cur_lexeme(1));
  break; case ')': add_token(TOK_RIGHT_PAREN, get_cur_lexeme(1));
  break; case '{': add_token(TOK_LEFT_CURLY, get_cur_lexeme(1));
  break; case '}': add_token(TOK_RIGHT_CURLY, get_cur_lexeme(1));
  break; case '[': add_token(TOK_LEFT_BRACE, get_cur_lexeme(1));
  break; case ']': add_token(TOK_RIGHT_BRACE, get_cur_lexeme(1));
  break; case '.': add_token(TOK_DOT, get_cur_lexeme(1));
  break; case '-': add_token(TOK_MINUS, get_cur_lexeme(1));
  break; case '+': add_token(TOK_PLUS, get_cur_lexeme(1));
  break; case ';': add_token(TOK_SEMICOLON, get_cur_lexeme(1));
  break; case '*': add_token(TOK_STAR, get_cur_lexeme(1));
  break; case '%': add_token(TOK_MODULO, get_cur_lexeme(1));
  break; case '/':
    if (peek_next() == '/') {
      move_while ([](char ch){return ch != '\n';});
      // ignore comments
    } else {
      add_token(TOK_SLASH, get_cur_lexeme(1));
    }
  break; case '!': 
    if (match('='))
      add_token(TOK_BANG_EQUAL, "!=");
    else add_token(TOK_BANG, "!");
  break; case '=': 
    if (match('='))
      add_token(TOK_EQUAL_EQUAL, "==");
    else add_token(TOK_EQUAL, "=");
  break; case '<': 
    if (match('='))
      add_token(TOK_LESS_EQUAL, "<=");
    else add_token(TOK_LESS, "<");
  break; case '>': 
    if (match('='))
      add_token(TOK_GREATER_EQUAL, ">=");
    else add_token(TOK_GREATER, ">");
  break;
  case ' ':
  case '\r':
  case '\t':
  break; case '\n': new_line();
  break; case '"': handle_string();
  break; default:
    if (std::isdigit(c))
      handle_number();
    else if (std::isalpha(c))
      handle_identifier();
    else
      throw_exception("Unexpected character.");
  }
  advance_lexeme();
}

void Lexer::handle_number() {
  bool point = false;
  move_while([this, &point](char c) mutable {
        if (c == '.') {
          if (point) throw_exception("Number with 2 floating points");

          point = !point;
          return point;
        }
        return std::isdigit(c) != 0;
      }); // Move while characters are digit (skips a single point)
  try {
    if (point) {
      double result;
      result = std::stod(get_cur_lexeme());
      add_token(TOK_NUMBER, get_cur_lexeme(), result);
    } else {
      uint32_t result;
      result = std::stoi(get_cur_lexeme());
      add_token(TOK_NUMBER, get_cur_lexeme(), result);
    }
  } catch (std::invalid_argument& e) {
    throw_exception(fmt::format("Couldn't parse a number '{}'", get_cur_lexeme()));
  } catch (std::out_of_range& e) {
    throw_exception(fmt::format("Number overflows size of double '{}'", get_cur_lexeme()));
  }
}

[[nodiscard]]
std::string Lexer::handle_escape_chars(const std::string& input) {
  std::string output{};
  std::for_each(input.cbegin(), input.cend(),[&output, escaped = false, this](char cur) mutable {
      if (escaped) {
        switch (cur) {
          break; case 'n':
            output += '\n';
          break; case 't':
            output += '\t';
          break; case '\\':
            output += '\\';
          break; default:
            throw_exception(fmt::format("unknown escape char '{}'", cur));
            break;
        };
        escaped = false;
        return;
      }
      if (cur == '\\') {
         escaped = true; 
         return;
      }
      output += cur;
      });
  return output;
}

void Lexer::handle_string() {
  move_while([quote = false](char c) mutable {
    if (c == '"') {
      quote = !quote;
      return quote;
    }
    return true;
  });

  if (is_end() && peek() != '"' && start_ind != current_ind) 
    throw_exception("End of string not found.");
  std::string lexeme = get_cur_lexeme(1);
  move_cursor();

  std::string str = lexeme.substr(1, lexeme.length() - 2);

  str = handle_escape_chars(str);

  add_token(TOK_STRING, lexeme, str);
}

void Lexer::handle_identifier() {
  move_while(isalphanumeric);
  auto cur_lex = get_cur_lexeme();
  try {
    auto keyword_type = keyword_lookup.at(cur_lex);
    switch (keyword_type) {
      break; case TOK_TRUE:
        add_token(TOK_BOOL, cur_lex, true);
      break; case TOK_FALSE:
        add_token(TOK_BOOL, cur_lex, false);
      break; default: 
        add_token(keyword_lookup.at(cur_lex), cur_lex, cur_lex);
    }
  } catch (std::out_of_range& e) {
    add_token(TOK_IDENTIFIER, cur_lex);
  }
}
