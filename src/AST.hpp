/// \file

#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <stack>
#include <string>
#include <variant>
#include <vector>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "lexer.hpp"

namespace AST {
struct Module;
struct ParamDecl;
struct FnProto;
struct Scope;

// Types
struct Typename;

using Type = std::variant<Typename>;
using TypePtr = std::unique_ptr<Type>;

// Top level
struct Extern;
struct FnDecl;

using TopLevel = std::variant<Extern, FnDecl>;
using TopLevelPtr = std::unique_ptr<TopLevel>;

// Statements
struct Break;
struct Continue;
struct Conditional;
struct ForLoop;
struct StatExpr;
struct Return;
struct VarDecl;

using Statement = std::variant<Conditional, ForLoop, StatExpr, Return, VarDecl,
                               Break, Continue>;
using StatementPtr = std::unique_ptr<Statement>;

// Expression
struct BinOperator;
struct UnOperator;
struct Literal;
struct Variable;
struct Grouping;
struct FnCall;

using Expression = std::variant<BinOperator, UnOperator, Literal, Variable,
                                Grouping, FnCall>;
using ExpressionPtr = std::unique_ptr<Expression>;

struct Module {
  Token name;
  std::vector<TopLevelPtr> functions;

  Module(const Token &_name, std::vector<TopLevelPtr> &&funcs);
};

struct Typename {
  Token id;
  Typename(const Token &);
  static TypePtr make(const Token &);
};

struct FnProto {
  Token id;
  std::vector<ParamDecl> parameters;
  std::optional<Token> return_type;

  FnProto(const Token &_id, std::vector<ParamDecl> &&_params,
          const std::optional<Token> &_return_type);
};

struct Scope {
  std::vector<StatementPtr> statements;
  Scope(std::vector<StatementPtr> &&);
};

// Statement Expression not lisp's "s-expression"
struct StatExpr {
  ExpressionPtr expr;
  StatExpr(ExpressionPtr &&);
  static StatementPtr make(ExpressionPtr &&);
};

struct Return {
  std::optional<ExpressionPtr> opt_expression;
  Return(std::optional<ExpressionPtr> &&_expression = std::nullopt);
  static StatementPtr
  make(std::optional<ExpressionPtr> &&_expression = std::nullopt);
};

struct Break {
  Token id;
  Break(const Token &);
  static StatementPtr make(const Token &);
};

struct Continue {
  Token id;
  Continue(const Token &);
  static StatementPtr make(const Token &);
};

struct ParamDecl {
  TypePtr type;
  std::optional<Token> id;

  ParamDecl(TypePtr &&, const std::optional<Token> &);
};

struct VarDecl {
  TypePtr type;
  Token id;
  ExpressionPtr expression;

  VarDecl(TypePtr &&_type, const Token &_id, ExpressionPtr &&_expr);
  static StatementPtr make(TypePtr &&, const Token &, ExpressionPtr &&_expr);
};

struct Literal {
  Token value;
  Literal(const Token &t);

  static ExpressionPtr make(const Token &);
};

struct Variable {
  Token id;
  Variable(const Token &t);

  static ExpressionPtr make(const Token &);
};

struct BinOperator {
  Token op;
  ExpressionPtr lhs, rhs;

  BinOperator(ExpressionPtr &&_lhs, const Token &t, ExpressionPtr &&_rhs);
  static ExpressionPtr make(ExpressionPtr &&_lhs, const Token &t,
                            ExpressionPtr &&_rhs);
};

struct UnOperator {
  Token op;
  ExpressionPtr rhs;
  UnOperator(const Token &t, ExpressionPtr &&_rhs);
  static ExpressionPtr make(const Token &t, ExpressionPtr &&_rhs);
};

struct Grouping {
  ExpressionPtr expr;

  Grouping(ExpressionPtr &&_expr);
  static ExpressionPtr make(ExpressionPtr &&_expr);
};

struct FnCall {
  Token id;
  std::vector<ExpressionPtr> args;
  FnCall(const Token &_id, std::vector<ExpressionPtr> &&input);
  static ExpressionPtr make(const Token &_id,
                            std::vector<ExpressionPtr> &&_args);
};

struct Indexing {
  Token id;
  ExpressionPtr lhs;
  ExpressionPtr index;

  Indexing(const Token &, ExpressionPtr &&, ExpressionPtr &&);
  static ExpressionPtr make(const Token &, ExpressionPtr &&, ExpressionPtr &&);
};

struct Extern {
  FnProto proto;
  Extern(FnProto &&_proto);
  static TopLevelPtr make(FnProto &&_proto);
};

struct FnDecl {
  FnProto proto;
  Scope body;
  FnDecl(FnProto &&_proto, Scope &&_body);
  static TopLevelPtr make(FnProto &&_proto, Scope &&_body);
};

struct Conditional {
  Token id;
  ExpressionPtr condition;

  Scope if_body;
  std::optional<Scope> else_body;

  Conditional(const Token &_id, ExpressionPtr &&_cond, Scope &&_if,
              std::optional<Scope> &&_else);

  static StatementPtr make(const Token &_id, ExpressionPtr &&_cond, Scope &&_if,
                           std::optional<Scope> &&_else);
};

struct ForLoop {
  std::optional<StatementPtr> variable;
  std::optional<ExpressionPtr> condition;
  std::optional<ExpressionPtr> iteration;

  Scope body;

  ForLoop(Scope &&, std::optional<StatementPtr> && = std::nullopt,
          std::optional<ExpressionPtr> && = std::nullopt,
          std::optional<ExpressionPtr> && = std::nullopt);

  static StatementPtr make(Scope &&,
                           std::optional<StatementPtr> && = std::nullopt,
                           std::optional<ExpressionPtr> && = std::nullopt,
                           std::optional<ExpressionPtr> && = std::nullopt);
};

} // namespace AST
