#include "AST.hpp"

namespace AST {

ParamDecl::ParamDecl(const Token& _type, const std::optional<Token>& _id) 
  : type(_type), id(_id) { }

FnProto::FnProto(const Token& _id, std::vector<ParamDecl>&& _params, const Token& _return_type) 
  : id (_id), return_type(_return_type) {
  for (auto& param : _params) {
    parameters.emplace_back(std::move(param));
  }
}

Scope::Scope(std::vector<StatementPtr>&& input) : statements(input.size()) { 
  for (size_t ind = 0;ind < input.size(); ++ind) {
    statements[ind].reset(input[ind].release());
  }
} 

StatExpr::StatExpr(ExpressionPtr&& _expr) 
  : expr(std::forward<ExpressionPtr>(_expr)) { }


StatementPtr StatExpr::make(ExpressionPtr&& _expr) {
  return std::make_unique<Statement>(
      StatExpr(std::forward<ExpressionPtr>(_expr)));
}

Return::Return(std::optional<ExpressionPtr>&& _expression) 
  : opt_expression(
      std::forward<std::optional<ExpressionPtr>>(_expression)) { }

StatementPtr Return::make(std::optional<ExpressionPtr>&& _expression) {
  return std::make_unique<Statement>(
      Return(std::forward<std::optional<ExpressionPtr>>(_expression)));
}

VarDecl::VarDecl(const Token& _type, const Token& _id, ExpressionPtr&& _expr) 
  : type(_type), 
    id(_id), 
    expression(std::forward<ExpressionPtr>(_expr)) { }

StatementPtr VarDecl::make(const Token& _type, const Token& _id, ExpressionPtr&& _expr) {
  return std::make_unique<Statement>(
      VarDecl(
        _type,
        _id,
        std::forward<ExpressionPtr>(_expr)));
}


Literal::Literal(const Token& t) : value(t) { }
ExpressionPtr Literal::make(const Token& t) {
  return std::make_unique<Expression>(
        Literal(t));
}

Variable::Variable(const Token& t) : id(t) { }
ExpressionPtr Variable::make(const Token& t) {
  return std::make_unique<Expression>(
        Variable(t));
}

BinOperator::BinOperator(
    ExpressionPtr&& _lhs,
    const Token& t,
    ExpressionPtr&& _rhs) 
  : op(t), 
    lhs(std::forward<ExpressionPtr>(_lhs)),
    rhs(std::forward<ExpressionPtr>(_rhs)) { }

ExpressionPtr BinOperator::make(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs) {
  return std::make_unique<Expression>(
        BinOperator(
          std::forward<ExpressionPtr>(_lhs),
          t,
          std::forward<ExpressionPtr>(_rhs)));
}

UnOperator::UnOperator(const Token& t, ExpressionPtr&& _rhs) 
  : op(t), rhs(std::forward<ExpressionPtr>(_rhs)) { }

ExpressionPtr UnOperator::make(const Token& t, ExpressionPtr&& _rhs) {
  return std::make_unique<Expression>(
        UnOperator(
          t,
          std::forward<ExpressionPtr>(_rhs)));
}

Grouping::Grouping(ExpressionPtr && _expr)
: expr(std::forward<ExpressionPtr>(_expr)) { }

ExpressionPtr Grouping::make(ExpressionPtr&& _expr) {
  return std::make_unique<Expression>(
        Grouping(std::forward<ExpressionPtr>(_expr)));
}

FnCall::FnCall(const Token& _id, std::vector<ExpressionPtr>&& input) 
  : id(_id), args(input.size()) { 
  for (size_t ind = 0; ind < input.size(); ++ind) {
    args[ind].reset(input[ind].release());
  }
}

ExpressionPtr FnCall::make(const Token& _id, std::vector<ExpressionPtr>&& _args) {
  return std::make_unique<Expression>(
        FnCall(
          _id,
          std::forward<std::vector<ExpressionPtr>>(_args)));
}

Extern::Extern(FnProto&& _proto)
  : proto(std::forward<FnProto>(_proto)) {}

TopLevelPtr Extern::make(FnProto&& _proto) {
  return std::make_unique<TopLevel>(
        Extern(std::forward<FnProto>(_proto)));
}

FnDecl::FnDecl(FnProto&& _proto, Scope&& _body)
  : proto(std::forward<FnProto>(_proto)), 
    body(std::forward<Scope>(_body)) { }

TopLevelPtr FnDecl::make(FnProto&& _proto, Scope&& _body) {
  return std::make_unique<TopLevel>(
    FnDecl(
      std::forward<FnProto>(_proto),
      std::forward<Scope>(_body)));
}

Conditional::Conditional(
    const Token& _id,
    ExpressionPtr&& _cond,
    Scope&& _if,
    std::optional<Scope>&& _else)
  : id(_id),
    condition(std::forward<ExpressionPtr>(_cond)),
    if_body(std::forward<Scope>(_if)),
    else_body(std::forward<std::optional<Scope>>(_else))
{ }

StatementPtr Conditional::make(
    const Token& _id,
    ExpressionPtr&& _cond,
    Scope&& _if,
    std::optional<Scope>&& _else) {
  return std::make_unique<Statement>(
    Conditional(
      _id,
      std::forward<ExpressionPtr>(_cond),
      std::forward<Scope>(_if),
      std::forward<std::optional<Scope>>(_else)));
}

ForLoop::ForLoop(
    Scope&& _body,
    std::optional<StatementPtr>&&  _var,
    std::optional<ExpressionPtr>&& _cond,
    std::optional<ExpressionPtr>&& _iter) 
  : variable(std::forward<std::optional<StatementPtr>>(_var)),
    condition(std::forward<std::optional<ExpressionPtr>>(_cond)),
    iteration(std::forward<std::optional<ExpressionPtr>>(_iter)),
    body(std::forward<Scope>(_body)) { }

StatementPtr ForLoop::make(
    Scope&& _body,
    std::optional<StatementPtr>&&  _var,
    std::optional<ExpressionPtr>&& _cond,
    std::optional<ExpressionPtr>&& _iter) {
  return std::make_unique<Statement>(
      ForLoop(
        std::forward<Scope>(_body),
        std::forward<std::optional<StatementPtr>>(_var),
        std::forward<std::optional<ExpressionPtr>>(_cond),
        std::forward<std::optional<ExpressionPtr>>(_iter)));
}


Module::Module(const Token& _name, std::vector<TopLevelPtr>&& funcs) 
  : name(_name), functions(funcs.size()) {
  for (size_t ind = 0; ind < funcs.size(); ++ind) {
    functions.at(ind).reset(funcs.at(ind).release());
  }
}

} // AST namespace

