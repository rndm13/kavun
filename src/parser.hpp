/// \file

#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "AST.hpp"
#include "lexer.hpp"

class parser_exception : std::exception {
  std::string info;
public:
  parser_exception(const Token& tok, const std::string& in)
  : info(fmt::format("('{}', line {}, col {}) : {}", tok.lexeme, tok.line, tok.col, in)) { }
  virtual const char* what() {
    return info.c_str();
  }
};

class Parser {
  std::vector<Token> tokens;
  size_t current_ind;

  std::vector<std::string> exception_stack;

  void move_cursor(size_t to_move = 1) {
    current_ind += to_move;
  }

  [[nodiscard]]
  bool is_end() { return current_ind + 1 >= tokens.size(); }

  Token peek(int offset = 0) {
    return tokens.at(std::min(current_ind + offset, tokens.size() - 1));
  }
  
  void throw_exception(const std::string& in) {
    throw parser_exception(peek(), in);
  }

  void assertion(bool condition, const std::string& message) {
    if (!condition) throw_exception(message);
  }

  AST::ParamDecl     handle_param_decl();
  AST::TopLevelPtr   handle_top_level();
  AST::TopLevelPtr   handle_fn_decl(); 
  AST::TopLevelPtr   handle_extern_fn(); 
  AST::FnProto       handle_fn_proto(); 
  AST::Scope         handle_scope(); 
  AST::StatementPtr  handle_statement(); 
  AST::StatementPtr  handle_return(); 
  AST::StatementPtr  handle_vd();
  AST::Module        handle_module();
  AST::StatementPtr  handle_conditional();
  AST::StatementPtr  handle_for();
  AST::ExpressionPtr handle_expr();
  AST::TypePtr       handle_type();

  // Expressions:
  AST::ExpressionPtr primary() noexcept;
  AST::ExpressionPtr unary() noexcept;
  AST::ExpressionPtr factor() noexcept;
  AST::ExpressionPtr term() noexcept;
  AST::ExpressionPtr comparison() noexcept;
  AST::ExpressionPtr equality() noexcept;
  AST::ExpressionPtr conjunction() noexcept;
  AST::ExpressionPtr disjunction() noexcept;
  AST::ExpressionPtr assignment() noexcept;

  // Types:
  AST::TypePtr       handle_typename();
  AST::TypePtr       handle_array_type();
  
  bool match(std::vector<TokenType>, int to_peek = 1);

public:
  AST::Module parse(std::vector<Token> input) {
    tokens = input;
    current_ind = 0;
    return handle_module();
  }
};
