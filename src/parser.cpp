#include "parser.hpp"

ModuleAST::Ptr Parser::handle_module() {
  assertion(peek().type == TOK_MODULE, "module must start with module declaration");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "module must have a name");
  auto name = peek();
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "missing ';'");
  move_cursor();

  auto result = take_with(&Parser::handle_fn_decl);

  move_cursor();
  if (current_ind + 1 < tokens.size()) {
    throw_exception(fmt::format("failed to parse entire file. exception stack:\n{}", fmt::join(exception_stack, "\n")));
  }
  return std::make_unique<ModuleAST>(name, std::move(result));
}

FunctionPrototypeAST::Ptr Parser::handle_fn_proto() {
  assertion(peek().type == TOK_FN, "function declaration must start with fn");
  move_cursor();
  auto identifier = peek();
  assertion(identifier.type == TOK_IDENTIFIER, "function name must be a valid identifier");
  move_cursor();
  assertion(peek().type == TOK_LEFT_PAREN, "function declaration parameters missing left parenthesis");
  move_cursor();
  auto params = take_with(&Parser::handle_vd);
  assertion(peek().type == TOK_RIGHT_PAREN, "function declaration parameters missing right parenthesis");
  return std::make_unique<FunctionPrototypeAST>(identifier, std::move(params)); 
}

FunctionDeclarationAST::Ptr Parser::handle_fn_decl() {
  auto proto = handle_fn_proto();
  move_cursor();
  auto scope = handle_scope();
  return std::make_unique<FunctionDeclarationAST>(std::move(proto), std::move(scope)); 
}

ScopeAST::Ptr Parser::handle_scope() {
  assertion(peek().type == TOK_LEFT_CURLY, "scope must start with a left curly brace");
  move_cursor();
  
  auto statements = take_with(&Parser::handle_statement);

  assertion(peek().type == TOK_RIGHT_CURLY, "scope must end with a right curly brace");
  return std::make_unique<ScopeAST>(std::move(statements));
}

StatementAST::Ptr Parser::handle_statement() {
  StatementAST::Ptr result;
  size_t old_ind = current_ind;
  try {
    result = handle_vd();
  } catch (parser_exception& e) {
    current_ind = old_ind;
    result = handle_expr();
  }
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
  return result; 
}

StatementAST::Ptr Parser::handle_vd() {
  assertion(peek().type == TOK_VAR, "statements must be either variable declaration or expression");
  move_cursor();
  auto id = peek();
  assertion(id.type == TOK_IDENTIFIER, "variable name must be a valid identifier");
  if (peek(1).type == TOK_EQUAL) {
    move_cursor(2); 
    auto expr = handle_expr();
    return std::make_unique<VariableDeclarationAST>(id, std::move(expr));
  }
  return std::make_unique<VariableDeclarationAST>(id, nullptr);
}

ExpressionAST::Ptr Parser::handle_expr() {
  // TODO: fix prioritization
  auto cur = peek();
  
  ExpressionAST::Ptr result = nullptr;
  if (cur.type == TOK_IDENTIFIER) { 
    if (peek(1).type == TOK_LEFT_PAREN) { // Function call
      Dprint("Function call\n");
      move_cursor(2);
      auto args = take_with(&Parser::handle_expr);
      assertion(peek().type == TOK_RIGHT_PAREN, "missing parentheses for function call");
      result = std::make_unique<FunctionCallAST>(cur, std::move(args));
    }
    else {
      Dprint("Variable\n");
      result = std::make_unique<VariableAST>(cur);
    }
  }

  if (cur.is_literal()) {
    Dprint("Literal\n");
    result = std::make_unique<LiteralAST>(cur);
  }

  if (cur.is_unary_op()) {
    Dprint("Unary\n");
    move_cursor();
    auto rhs = handle_expr();
    result = std::make_unique<UnaryOperationAST>(cur, std::move(rhs));
  }

  if (cur.type == TOK_LEFT_PAREN) {
    Dprint("Grouping\n");
    move_cursor();
    auto expr = handle_expr();
    move_cursor();
    assertion(peek().type == TOK_RIGHT_PAREN, "missing parentheses for grouping");
    result = std::make_unique<GroupingAST>(std::move(expr));
  }

  // -2 + 3
  // right now: -(2 + 3)
  // correct: (-2) + 3

  auto next = peek(1);

  if (result && next.is_binary_op()) {
    Dprint("Binary {} \n", next.lexeme);
    move_cursor(2);
    auto rhs = handle_expr();
    result = std::make_unique<BinaryOperationAST>(std::move(result), next, std::move(rhs));
  }

  balance_expr(result);
  
  if (!result)
    throw_exception("failed to parse expression");
  return result;
}

#define CAST(to_cast, type) &dynamic_cast<type&>(*to_cast.get())

void Parser::balance_unary(ExpressionAST::Ptr& expr) {
  if (!expr) return;
  // if expression is unary and rhs is binary then
  // expression = 
  //   Binary(Unary(expression -> op, expression -> rhs -> lhs),
  //          expression -> rhs -> op,
  //          expression rhs -> rhs);

  try {
    UnaryOperationAST*  un_expr = CAST(expr, UnaryOperationAST);
    BinaryOperationAST* bin_rhs = CAST(un_expr -> rhs, BinaryOperationAST);
    balance_expr(bin_rhs -> rhs);
    balance_expr(un_expr -> rhs);
    expr = std::make_unique<BinaryOperationAST>(
      std::make_unique<UnaryOperationAST>(un_expr -> op, std::move(bin_rhs -> lhs)),
      bin_rhs -> op,
      std::move(bin_rhs -> rhs));
  } catch (std::bad_cast& e) {
    // TODO:
  }
}

#undef CAST
