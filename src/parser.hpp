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
  std::stack<std::size_t>  exc_stack_breakpoints;

  void move_cursor(size_t to_move = 1) {
    current_ind += to_move;
  }

  [[nodiscard]]
  bool is_end() { return current_ind + 1 >= tokens.size(); }

  Token peek(int offset = 0) {
    return tokens.at(std::min(current_ind + offset, tokens.size() - 1));
  }
  
  template<typename Predicate>
  std::vector<Token> take_while(Predicate&& pred) {
    std::vector<Token> result{};
    while (pred(peek())) {
      result.push_back(peek());
      move_cursor();
    }
    return result;
  }

  template<typename ReturnType>
  std::vector<ReturnType> take_with(ReturnType (Parser::*member_func)(), bool clear_excstack = true) {
    if (clear_excstack)
      set_excstack_breakpoint();
    std::vector<ReturnType> result{};
    size_t old_ind = current_ind;
    while (!is_end()) {
      try {
        result.push_back((this ->* member_func)());
        move_cursor(); // move to next
        old_ind = current_ind;
      } catch (parser_exception& e) {
        current_ind = old_ind;
        break;
      }
    }
    if (clear_excstack)
      break_excstack();
    return result;
  }  

  void set_excstack_breakpoint() {
    exc_stack_breakpoints.push(exception_stack.size());
  }
  
  void break_excstack() {
    if (exc_stack_breakpoints.empty()) 
      return;

    exception_stack.resize(exc_stack_breakpoints.top());
    exc_stack_breakpoints.pop();
  }

  void throw_exception(const std::string& in) {
    parser_exception e(peek(), in);
    exception_stack.push_back(e.what());
    throw parser_exception(peek(), in);
  }

  void assertion(bool condition, const std::string& message) {
    if (!condition) throw_exception(message);
  }

  AST::TopLevelPtr   handle_top_level();
  AST::TopLevelPtr   handle_fn_decl(); 
  AST::TopLevelPtr   handle_extern_fn(); 
  AST::FnProto       handle_fn_proto(); 
  AST::Scope         handle_scope(); 
  AST::StatementPtr  handle_statement(); 
  AST::StatementPtr  handle_return(); 
  AST::VarDecl       handle_vd();
  AST::Module        handle_module();
  AST::StatementPtr  handle_conditional();
  AST::ExpressionPtr handle_expr();

  // Expressions:
  AST::ExpressionPtr primary();
  AST::ExpressionPtr unary();
  AST::ExpressionPtr factor();
  AST::ExpressionPtr term();
  AST::ExpressionPtr comparison();
  AST::ExpressionPtr equality();
  AST::ExpressionPtr conjunction();
  AST::ExpressionPtr disjunction();
  
  bool match(std::vector<TokenType>);

public:
  AST::Module parse(std::vector<Token> input) {
    tokens = input;
    current_ind = 0;
    return handle_module();
  }
};
