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

bool Parser::match(std::vector<TokenType> to_match, int to_peek) {
  for (auto tok : to_match) {
    if (peek(to_peek).type == tok) {
      move_cursor();
      return true;
    }
  }
  return false;
}

AST::ExpressionPtr Parser::handle_expr() {
  return disjunction();
}

AST::ExpressionPtr Parser::disjunction() {
  auto expr = conjunction();

  while (match({TOK_OR})) {
    auto op = peek(0);
    move_cursor();
    auto right = conjunction();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::conjunction() {
  auto expr = equality();

  while (match({TOK_AND})) {
    auto op = peek();
    move_cursor();
    auto right = equality();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::equality() {
  auto expr = comparison();

  while (match({TOK_EQUAL_EQUAL, TOK_BANG_EQUAL})) {
    auto op = peek();
    move_cursor();
    auto right = comparison();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::comparison() {
  auto expr = term();

  while (match({TOK_LESS, TOK_LESS_EQUAL, TOK_GREATER, TOK_GREATER_EQUAL})) {
    auto op = peek();
    move_cursor();
    auto right = term();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::term() {
  auto expr = factor();

  while (match({TOK_PLUS, TOK_MINUS})) {
    auto op = peek();
    move_cursor();
    auto right = factor();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::factor() {
  auto expr = unary();

  while (match({TOK_MODULO, TOK_STAR, TOK_SLASH})) {
    auto op = peek();
    move_cursor();
    auto right = unary();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::unary() {
  if (match({TOK_MINUS, TOK_BANG}, 0)) {
    auto op = peek(-1);
    auto right = unary();
    return AST::UnOperator::make(
        op,
        std::forward<AST::ExpressionPtr>(right));
  }
  return primary();
}

AST::ExpressionPtr Parser::primary() {
  auto tok = peek();
  if (tok.type == TOK_NUMBER ||
      tok.type == TOK_STRING || 
      tok.type == TOK_BOOL) {
    return AST::Literal::make(tok);
  }

  if (tok.type == TOK_IDENTIFIER) {
    if (peek(1).type == TOK_LEFT_PAREN) {
      move_cursor(2); // Skipping over parenthesis
      auto args = take_with(&Parser::handle_expr);
      assertion(peek().type == TOK_RIGHT_PAREN, "missing right parenthesis");
      return AST::FnCall::make(
          tok,
          std::forward<std::vector<AST::ExpressionPtr>>(args));
    }
    return AST::Variable::make(tok);
  }
  if (tok.type == TOK_LEFT_PAREN) {
    move_cursor();
    auto expr = handle_expr();
    move_cursor();
    assertion(peek().type == TOK_RIGHT_PAREN, "missing right parenthesis");
    return AST::Grouping::make(
        std::forward<AST::ExpressionPtr>(expr));
  }
  throw_exception("failed to parse expression");
  return nullptr;
}
