#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>
#include <stack>
#include <optional>
#include <variant>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "lexer.hpp"

namespace AST {
struct Module;

struct ParamDecl;
struct FnProto;
struct Scope;

// Top level
struct Extern;
struct FnDecl;

// Statements
struct Conditional;
struct StatExpr;
struct Return;
struct VarDecl;

// Expression
struct BinOperator;
struct UnOperator;
struct Literal;
struct Variable;
struct Grouping;
struct FnCall;

using TopLevel = std::variant
  <Extern, FnDecl>;
using TopLevelPtr = std::unique_ptr<TopLevel>;

using Statement = std::variant
  <Conditional, StatExpr, Return, VarDecl>;
using StatementPtr = std::unique_ptr<Statement>;
  
using Expression = std::variant
  <BinOperator, UnOperator, Literal, Variable, Grouping, FnCall>;
using ExpressionPtr = std::unique_ptr<Expression>;

struct FnProto {
  Token id;
  std::vector<ParamDecl> parameters;
  Token return_type;

  FnProto(const Token& _id, std::vector<ParamDecl>&& _params, const Token& _return_type);
};

struct Scope {
  std::vector<StatementPtr> statements;
  Scope(std::vector<StatementPtr>&&);
};

// Statement Expression not lisp's "s-expression"
struct StatExpr { 
  ExpressionPtr expr;
  StatExpr(ExpressionPtr&&);
  static StatementPtr make(ExpressionPtr&&);
};

struct Return {
  std::optional<ExpressionPtr> opt_expression;
  Return(std::optional<ExpressionPtr>&& _expression = std::nullopt);
  static StatementPtr make(std::optional<ExpressionPtr>&& _expression = std::nullopt);
};

struct ParamDecl {
  Token type;
  std::optional<Token> id;

  ParamDecl(const Token&, const std::optional<Token>&);
};

struct VarDecl {
  Token type;
  Token id;
  ExpressionPtr expression;

  VarDecl(const Token& _type, const Token& _id, ExpressionPtr&& _expr);
  static StatementPtr make(const Token&, const Token&, ExpressionPtr&& _expr);
};

struct Literal {
  Token value;
  Literal(const Token& t);

  static ExpressionPtr make(const Token&);
};

struct Variable {
  Token id;
  Variable(const Token& t);

  static ExpressionPtr make(const Token&);
};

struct BinOperator{
  Token op;
  ExpressionPtr lhs, rhs;

  BinOperator(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs);
  static ExpressionPtr make(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs);
};

struct UnOperator {
  Token op;
  ExpressionPtr  rhs;
  UnOperator(const Token& t, ExpressionPtr && _rhs) ;
  static ExpressionPtr make(const Token& t, ExpressionPtr&& _rhs);
};

struct Grouping {
  ExpressionPtr  expr;

  Grouping(ExpressionPtr&& _expr);
  static ExpressionPtr make(ExpressionPtr&& _expr);
};

struct FnCall {
  Token id;
  std::vector<ExpressionPtr> args;
  FnCall(const Token& _id, std::vector<ExpressionPtr>&& input);
  static ExpressionPtr make(const Token& _id, std::vector<ExpressionPtr>&& _args);
};

struct Extern {
  FnProto proto;
  Extern(FnProto&& _proto);
  static TopLevelPtr make(FnProto&& _proto);
};

struct FnDecl {
  FnProto proto;
  Scope body;
  FnDecl(FnProto&& _proto, Scope&& _body);
  static TopLevelPtr make(FnProto&& _proto, Scope&& _body);
};

struct Conditional {
  Token id;
  ExpressionPtr condition;

  Scope if_body;
  std::optional<Scope> else_body;

  Conditional(
      const Token& _id,
      ExpressionPtr&& _cond,
      Scope&& _if,
      std::optional<Scope>&& _else = std::nullopt);

  static StatementPtr make(
      const Token& _id,
      ExpressionPtr&& _cond,
      Scope&& _if,
      std::optional<Scope>&& _else = std::nullopt);
};

struct Module {
  Token name;
  std::vector<TopLevelPtr> functions;

  Module(const Token& _name, std::vector<TopLevelPtr>&& funcs);

  // std::unique_ptr<llvm::Module> the_module;
};

} // AST namespace

