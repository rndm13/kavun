#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "lexer.hpp"

// Scope = '{' Statement* '}' 
// 
// Statement = (VariableDeclaration | Expression) ';' 
//
// a*  = a a*
//     |  
// 
// VariableDeclaration = 'var' Identifier ('=' Expression)? 
//
// Expression = Literal 
//            | Variable
//            | BinaryOperation
//            | FunctionCall
//            | UnaryOperation
//            | Grouping
//
// FunctionDeclaration = 'fn' Identifier '(' Identifier* ')' Scope
// 
// Literal = Number 
//         | String 
//         | Bool 
//         | Null
// 
// BinaryOperation = Expression BinaryOperator Expression 
//
// BinaryOperator  = '+' | '-' | '/'  | '*'  | '='  | 'and' | 'or'
//                 | '>' | '<' | '>=' | '<=' | '=='
//
// FunctionCall = Identifier '(' Expression* ')'
// 
// UnaryOperation = UnaryOperator Expression 
// UnaryOperator = '-'
//  
// Grouping = '(' Expression ')' 
//
// Program = FunctionDeclaration*

// TODO: Move all ASTs to seperate file

struct AST { 
  typedef std::unique_ptr<AST> Ptr;
  virtual std::string pretty_show() const = 0;
  virtual ~AST() { }
};

struct StatementAST : AST {
  typedef std::unique_ptr<StatementAST> Ptr;
};

struct ScopeAST : AST {
  std::vector<StatementAST::Ptr> statements;
  ScopeAST(std::vector<StatementAST::Ptr>&& input) : statements(input.size()) { 
    for (size_t ind = 0;ind < input.size(); ++ind) {
      statements[ind].reset(input[ind].release());
    }
  } 

  std::string pretty_show() const {
    std::string result = "ScopeAST {\n";
    for (auto& st : statements) {
      result += st -> pretty_show() + "\n";
    }
    result += "}\n";
    return result;
  }


  typedef std::unique_ptr<ScopeAST> Ptr;
};

struct ExpressionAST : StatementAST {
  typedef std::unique_ptr<ExpressionAST> Ptr; 
  virtual int get_priority() const = 0;
};

struct VariableDeclarationAST : StatementAST {
  Token id;
  ExpressionAST::Ptr opt_expression{nullptr};

  VariableDeclarationAST(Token _id, ExpressionAST::Ptr&& _expr) : id(_id) {
    opt_expression.reset(_expr.release());
  }

  std::string pretty_show() const {
    if (opt_expression)
      return fmt::format("VariableDeclarationAST {} = {}", id.lexeme, opt_expression -> pretty_show());
    return fmt::format("VariableDeclarationAST {}", id.lexeme);
  }
};

struct LiteralAST : ExpressionAST {
  Token value;
  LiteralAST(const Token& t) : value(t) { }

  std::string pretty_show() const {
    return fmt::format("LiteralAST {}", value.lexeme);
  }
  int get_priority() const {
    return 0;
  }
};

struct VariableAST : ExpressionAST {
  Token id;
  VariableAST(const Token& t) : id(t) { }

  std::string pretty_show() const {
    return fmt::format("VariableAST {}", id.lexeme);
  }

  int get_priority() const {
    return 0;
  }
};

struct BinaryOperationAST : ExpressionAST {
  ExpressionAST::Ptr lhs, rhs;
  Token op;

  BinaryOperationAST(ExpressionAST::Ptr&& _lhs, const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    lhs.reset(_lhs.release());
    rhs.reset(_rhs.release());
  }

  std::string pretty_show() const {
    // return fmt::format("BinaryOperationAST {} {} {}", lhs -> pretty_show(), op.lexeme, rhs -> pretty_show());
    return fmt::format("({} {} {})", lhs -> pretty_show(), op.lexeme, rhs -> pretty_show());
  }

  int get_priority() const {
    // TODO:
    return 0;
  }
};

struct UnaryOperationAST : ExpressionAST {
  Token op;
  ExpressionAST::Ptr rhs;
  UnaryOperationAST(const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    rhs.reset(_rhs.release());
  }

  std::string pretty_show() const {
    // return fmt::format("UnaryOperationAST {} {}", op.lexeme, rhs -> pretty_show());
    return fmt::format("({} {})", op.lexeme, rhs -> pretty_show());
  }

  int get_priority() const {
    return 99999;
  }
};

struct GroupingAST : ExpressionAST {
  ExpressionAST::Ptr expr;

  GroupingAST(ExpressionAST::Ptr&& _expr) {
    expr.reset(_expr.release());
  }

  std::string pretty_show() const {
    return fmt::format("GroupingAST ({})", expr -> pretty_show());
  }

  int get_priority() const {
    return 99999;
  }
};

struct FunctionCallAST : ExpressionAST {
  Token id;
  std::vector<ExpressionAST::Ptr> args;
  FunctionCallAST(const Token& _id, std::vector<ExpressionAST::Ptr>&& input) : id(_id), args(input.size()) { 
    for (size_t ind = 0; ind < input.size(); ++ind) {
      args[ind].reset(input[ind].release());
    }
  }

  std::string pretty_show() const {
    std::vector<std::string> args_str(args.size());
    for (size_t ind = 0; ind < args.size(); ++ind) {
      args_str.at(ind) = args.at(ind) -> pretty_show();
    }
    return fmt::format("FunctionCallAST {} ({})", id.lexeme, fmt::join(args_str, " "));
  }

  int get_priority() const {
    return 9999;
  }
};

struct FunctionDeclarationAST : AST {
  Token id;
  std::vector<Token> parameters;
  ScopeAST::Ptr body;
  FunctionDeclarationAST(const Token& _id, const std::vector<Token>& _params, ScopeAST::Ptr&& _body)
    : id(_id), parameters(_params) { 
    body.reset(_body.release());
  }

  std::string pretty_show() const {
    std::vector<std::string> params(parameters.size());
    for (size_t ind = 0; ind < parameters.size(); ++ind) {
      params.at(ind) = parameters.at(ind).lexeme;
    }
    return fmt::format("FunctionDeclarationAST {} ({}) {}", id.lexeme, fmt::join(params, " "), body -> pretty_show());
  }
};

struct ModuleAST : AST {
  std::vector<FunctionDeclarationAST::Ptr> functions;

  ModuleAST(std::vector<FunctionDeclarationAST::Ptr>&& funcs) : functions(funcs.size()) {
    for (size_t ind = 0; ind < funcs.size(); ++ind) {
      functions.at(ind).reset(funcs.at(ind).release());
    }
  }

  std::string pretty_show() const {
    std::vector<std::string> func_str(functions.size());
    for (size_t ind = 0; ind < functions.size(); ++ind) {
      func_str.at(ind) = functions.at(ind) -> pretty_show();
    }
    return fmt::format("{}", fmt::join(func_str, "\n")); 
  }

  typedef std::unique_ptr<ModuleAST> Ptr; 
};

