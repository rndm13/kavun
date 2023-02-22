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

#ifdef DEBUG
#define Dprint fmt::print
#else 
#define Dprint 
#endif
 
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
  std::vector<ReturnType> take_with(ReturnType (Parser::*member_func)()) {
    std::vector<ReturnType> result{};
    size_t old_ind = current_ind;
    while (!is_end()) {
      try {
        result.push_back((this ->* member_func)());
        move_cursor(); // move to next
        old_ind = current_ind;
      } catch (parser_exception& e) {
        current_ind = old_ind;
        Dprint("Break\n");
        break;
      }
    }
    return result;
  }  

  void throw_exception(const std::string& in) {
    parser_exception e(peek(), in);
    exception_stack.push_back(e.what());
    throw parser_exception(peek(), in);
  }

  void assertion(bool condition, const std::string& message) {
    if (!condition) throw_exception(message);
  }

  TopLevelAST::Ptr handle_top_level();
  FunctionDeclarationAST::Ptr handle_fn_decl(); 
  ExternFunctionAST::Ptr handle_extern_fn(); 
  FunctionPrototypeAST::Ptr handle_fn_proto(); 
  ScopeAST::Ptr handle_scope(); 
  StatementAST::Ptr handle_statement(); 
  ReturnAST::Ptr handle_return(); 
  VariableDeclarationAST::Ptr handle_vd();
  ModuleAST::Ptr handle_module();
  ConditionalAST::Ptr handle_conditional();

  ExpressionAST::Ptr handle_expr();
  
  void balance_expr(ExpressionAST::Ptr& expr) { balance_unary(expr); }
  void balance_unary(ExpressionAST::Ptr& expr);
public:
  ModuleAST::Ptr parse(std::vector<Token> input) {
    tokens = input;
    current_ind = 0;
    return handle_module();
  }
};
