#pragma once

#include <vector>
#include <string>
#include <memory>
#include "lexer.hpp"

// Scope = '{' Statement* '}' +++
//
// Statement = (VariableDeclaration | Expression) ';' +++
//
// a*  = a a*
//     |  
//
// [a] = a, [a]
//     | a
//     | 
//
// VariableDeclaration = 'var' Identifier ('=' Expression)? +++
//
// Expression = Literal 
//            | BinaryOperation
//            | FunctionCall
//            | UnaryOperation
//            | Grouping
//
// FunctionDeclaration = 'fn' Identifier '(' [Identifier] ')' Scope
//
// Literal = Number +++
//         | String 
//         | Bool 
//         | Null
//
// BinaryOperation = Expression BinaryOperator Expression +++
// BinaryOperator = '+' | '-' | '/'  | '*'  | '='  | 'and' | 'or'
//                | '>' | '<' | '>=' | '<=' | '=='
//
// FunctionCall = Identifier '(' [Expression] ')'
//
// UnaryOperation = UnaryOperator Expression +++
// UnaryOperator = '-'
// 
// Grouping = '(' Expression ')' +++

// TODO: Move all ASTs to seperate file

struct AST {
  typedef std::unique_ptr<AST> Ptr;
};

struct StatementAST : AST {
  typedef std::unique_ptr<StatementAST> Ptr;
};

struct ScopeAST : AST {
  std::vector<StatementAST> statements;
  
  typedef std::unique_ptr<ScopeAST> Ptr;
};

typedef std::string Identifier;

struct ExpressionAST : StatementAST {
  typedef std::unique_ptr<ExpressionAST> Ptr; 
};

struct VariableDeclarationAST : StatementAST {
  Identifier id{};
  ExpressionAST::Ptr opt_expression{nullptr};
};

struct LiteralAST : ExpressionAST {
  Token value;
};

struct BinaryOperationAST : ExpressionAST {
  ExpressionAST::Ptr lhs, rhs;
  Token op;
};

struct UnaryOperationAST : ExpressionAST {
  ExpressionAST::Ptr rhs;
  Token op;
};

struct GroupingAST : ExpressionAST {
  ExpressionAST::Ptr expr;
};

struct FunctionCallAST : ExpressionAST {
  Token id;
  std::vector<ExpressionAST::Ptr> args;
};

struct FunctionDeclarationAST : AST {
  Token id;
  std::vector<Token> parameters;
  ScopeAST::Ptr body;
};

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
  size_t current_token;

  AST::Ptr parse(std::vector<Token> _tokens) {
    tokens = _tokens;
    current_token = 0;
    return nullptr;
  }

  void throw_exception(const std::string& in) {
    throw parser_exception(tokens.at(current_token), in);
  }
}
