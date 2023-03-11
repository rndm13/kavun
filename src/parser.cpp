#include "parser.hpp"

AST::Module Parser::handle_module() {
  set_excstack_breakpoint();
  assertion(peek().type == TOK_MODULE, "module must start with module declaration");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "module must have a name");
  auto name = peek();
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "missing ';'");
  move_cursor();

  auto result = take_with(&Parser::handle_top_level, false);

  move_cursor();
  if (current_ind + 1 < tokens.size()) {
    throw_exception(fmt::format("failed to parse entire file. exception stack:\n{}", fmt::join(exception_stack, "\n")));
  }
  break_excstack();
  return AST::Module(name, std::move(result));
}

AST::TopLevelPtr Parser::handle_top_level() {
  set_excstack_breakpoint();
  AST::TopLevelPtr result;
  size_t old_ind = current_ind;
  try {
    result = handle_fn_decl();
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    current_ind = old_ind;
  }
  try {
    result = handle_extern_fn();
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    throw_exception("failed to parse top level");
    return nullptr;
  }
}

AST::FnProto Parser::handle_fn_proto() {
  assertion(peek().type == TOK_FN, "function prototype must start with fn");
  move_cursor();
  auto identifier = peek();
  assertion(identifier.type == TOK_IDENTIFIER, "function name must be a valid identifier");
  move_cursor();
  assertion(peek().type == TOK_LEFT_PAREN, "function prototype parameters missing left parenthesis");
  move_cursor();
  auto params = take_with(&Parser::handle_vd);
  assertion(peek().type == TOK_RIGHT_PAREN, "function prototype parameters missing right parenthesis");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "function prototype must have a return type");
  auto return_type = peek();
  return AST::FnProto(
      identifier, 
      std::forward<std::vector<AST::VarDecl>>(params),
      return_type); 
}

AST::TopLevelPtr Parser::handle_extern_fn() {
  assertion(peek().type == TOK_EXTERN, "external function declaration must start with 'extern'");
  move_cursor();
  auto proto = handle_fn_proto();
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "missing semicolon");
  return AST::Extern::make(
      std::forward<AST::FnProto>(proto)); 
}

AST::TopLevelPtr Parser::handle_fn_decl() {
  auto proto = handle_fn_proto();
  move_cursor();
  auto scope = handle_scope();
  return AST::FnDecl::make(
      std::forward<AST::FnProto>(proto),
      std::forward<AST::Scope>(scope)); 
}

AST::Scope Parser::handle_scope() {
  assertion(peek().type == TOK_LEFT_CURLY, "scope must start with a left curly brace");
  move_cursor();
  
  auto statements = take_with(
      &Parser::handle_statement, false);

  assertion(peek().type == TOK_RIGHT_CURLY, "scope must end with a right curly brace");
  return AST::Scope(
      std::forward<std::vector<AST::StatementPtr>>(statements));
}

AST::StatementPtr Parser::handle_statement() {
  set_excstack_breakpoint();
  AST::StatementPtr result;
  size_t old_ind = current_ind;
  try {
    result = std::make_unique<AST::Statement>(handle_vd());
    move_cursor();
    assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    current_ind = old_ind;
  } 
  try {
    auto expr = handle_expr();
    result = AST::StatExpr::make(std::forward<AST::ExpressionPtr>(expr));
    move_cursor();
    assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    current_ind = old_ind;
  }
  try {
    result = handle_return();
    move_cursor();
    assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    current_ind = old_ind;
  }
  try {
    result = handle_conditional();
    break_excstack();
    return result; 
  } catch (parser_exception& e) {
    throw_exception("failed to parse statement");
    return nullptr;
  }
}

AST::StatementPtr Parser::handle_conditional() {
  assertion(peek().type == TOK_IF, "conditional must start with if");
  auto id = peek(); 
  move_cursor();
  auto cond = handle_expr();
  move_cursor();
  auto if_body = handle_scope();
  std::optional<AST::Scope> else_body = std::nullopt;
  if (peek(1).type == TOK_ELSE) {
    move_cursor(2);
    else_body = handle_scope();
  }
  return AST::Conditional::make(
      id,
      std::forward<AST::ExpressionPtr>(cond),
      std::forward<AST::Scope>(if_body),
      std::forward<std::optional<AST::Scope>>(else_body));
}

AST::StatementPtr Parser::handle_return() {
  assertion(peek().type == TOK_RETURN, "return must start with 'return'");
  if (peek(1).type == TOK_SEMICOLON)
    return AST::Return::make();
  move_cursor();
  auto expr = handle_expr();
  return AST::Return::make(std::forward<AST::ExpressionPtr>(expr));
}

AST::VarDecl Parser::handle_vd() {
  assertion(peek().type == TOK_VAR, "variable declaration must start with 'var'");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "variable declaration must have a type");
  auto type = peek();
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "variable name must be a valid identifier");
  auto id = peek();
  if (peek(1).type == TOK_EQUAL) {
    move_cursor(2); 
    auto expr = handle_expr();
    return AST::VarDecl(type, id, std::move(expr));
  }
  return AST::VarDecl(type, id, std::nullopt);
}

AST::ExpressionPtr Parser::handle_expr() {
  // TODO: fix prioritization
  auto cur = peek();
  
  AST::ExpressionPtr result = nullptr;
  if (cur.type == TOK_IDENTIFIER) { 
    if (peek(1).type == TOK_LEFT_PAREN) { // Function call
      move_cursor(2);
      auto args = take_with(&Parser::handle_expr);
      assertion(peek().type == TOK_RIGHT_PAREN, "missing parentheses for function call");
      result = AST::FnCall::make(
          cur, 
          std::forward<std::vector<AST::ExpressionPtr>>(args));
    }
    else {
      result = AST::Variable::make(cur);
    }
  }

  if (cur.is_literal()) {
    result = AST::Literal::make(cur);
  }

  if (cur.is_unary_op()) {
    move_cursor();
    auto rhs = handle_expr();
    result = AST::UnOperator::make(
        cur, 
        std::forward<AST::ExpressionPtr>(rhs));
  }

  if (cur.type == TOK_LEFT_PAREN) {
    move_cursor();
    auto expr = handle_expr();
    move_cursor();
    assertion(peek().type == TOK_RIGHT_PAREN, "missing parentheses for grouping");
    result = AST::Grouping::make(
        std::forward<AST::ExpressionPtr>(expr));
  }

  // -2 + 3
  // right now: -(2 + 3)
  // correct: (-2) + 3

  auto next = peek(1);

  if (result && next.is_binary_op()) {
    move_cursor(2);
    auto rhs = handle_expr();
    result = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(result), 
        next, 
        std::forward<AST::ExpressionPtr>(rhs));
  }

  // balance_expr(result);
  
  if (!result)
    throw_exception("failed to parse expression");
  return result;
}

// #define CAST(to_cast, type) &dynamic_cast<type&>(*to_cast.get())
// 
// void Parser::balance_unary(ExpressionAST::Ptr& expr) {
//   if (!expr) return;
//   // if expression is unary and rhs is binary then
//   // expression = 
//   //   Binary(Unary(expression -> op, expression -> rhs -> lhs),
//   //          expression -> rhs -> op,
//   //          expression rhs -> rhs);
// 
//   try {
//     UnaryOperationAST*  un_expr = CAST(expr, UnaryOperationAST);
//     BinaryOperationAST* bin_rhs = CAST(un_expr -> rhs, BinaryOperationAST);
//     balance_expr(bin_rhs -> rhs);
//     balance_expr(un_expr -> rhs);
//     expr = std::make_unique<BinaryOperationAST>(
//       std::make_unique<UnaryOperationAST>(un_expr -> op, std::move(bin_rhs -> lhs)),
//       bin_rhs -> op,
//       std::move(bin_rhs -> rhs));
//   } catch (std::bad_cast& e) {
//     balance_binary_precedence(expr);
//   }
// }
// 
// void Parser::balance_binary_precedence(ExpressionAST::Ptr& expr) {
//   if (!expr) return;
// 
//   // 2 * 3 + 4 * 5
//   // 2 * (3 + (4 * 5))
//   // (2 * 3) + (4 * 5)
//   
//   // if expression is binary and rhs is binary then
//   // if this precedence is lower than rhs precedence then
//   // expression = 
//   // rhs = Binary(
//   //  Binary(
//   //    this -> lhs,
//   //    this -> op,
//   //    rhs -> lhs), 
//   //  rhs -> op,
//   //  rhs -> rhs))
//   // 
// 
//   try {
//     BinaryOperationAST* this_expr = CAST(expr, BinaryOperationAST);
//     BinaryOperationAST* rhs_expr = CAST(this_expr -> rhs, BinaryOperationAST);
//     balance_expr(this_expr -> lhs);
//     balance_expr(this_expr -> rhs);
//     balance_expr(rhs_expr -> rhs);
// 
//     if (this_expr -> get_precedence() < rhs_expr -> get_precedence()) {
//       expr = std::make_unique<BinaryOperationAST>(
//         std::make_unique<BinaryOperationAST>(
//           std::move(this_expr -> lhs),
//           this_expr -> op,
//           std::move(rhs_expr -> lhs)),
//         rhs_expr -> op,
//         std::move(rhs_expr -> rhs));
//     }
//   } catch (std::bad_cast& e) { }
// }
// 
// #undef CAST
