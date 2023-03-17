#include "parser.hpp"

AST::Module Parser::handle_module() {
  assertion(peek().type == TOK_MODULE, "module must start with module declaration");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "module must have a name");
  auto name = peek();
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "missing semicolon");
  move_cursor();

  std::vector<AST::TopLevelPtr> result{};
  while (!is_end()) {
    result.push_back(handle_top_level());
    move_cursor();
  }
  return AST::Module(name, std::move(result));
}

AST::TopLevelPtr Parser::handle_top_level() {
  if (peek().type == TOK_FN) {
    return handle_fn_decl();
  } 
  if (peek().type == TOK_EXTERN) {
    return handle_extern_fn();
  } 
  throw_exception("failed to parse top level");
}

AST::ParamDecl Parser::handle_param_decl() {
  assertion(peek().type == TOK_IDENTIFIER, "parameter declaration must have a type");
  auto type = peek();
  std::optional<Token> name;
  if (peek(1).type == TOK_IDENTIFIER) {
    move_cursor();
    name = peek();
  }
  return AST::ParamDecl(type, name);
}

AST::FnProto Parser::handle_fn_proto() {
  assertion(peek().type == TOK_FN, "function prototype must start with fn");
  move_cursor();
  auto identifier = peek();
  assertion(identifier.type == TOK_IDENTIFIER, "function name must be a valid identifier");
  move_cursor();
  assertion(peek().type == TOK_LEFT_PAREN, "function prototype parameters missing left parenthesis");
  move_cursor();
  std::vector<AST::ParamDecl> params;

  while(!is_end()) {
    if (peek().type == TOK_RIGHT_PAREN)
      break;
    params.push_back(handle_param_decl());
    move_cursor();
    if (peek().type == TOK_RIGHT_PAREN)
      break;
    assertion(peek().type == TOK_COMMA, "missing comma");
    move_cursor();
  }

  assertion(peek().type == TOK_RIGHT_PAREN, "function prototype parameters missing right parenthesis");
  move_cursor();
  // TODO: make return type optional
  assertion(peek().type == TOK_IDENTIFIER, "function prototype must have a return type");
  auto return_type = peek();
  return AST::FnProto(
      identifier, 
      std::forward<std::vector<AST::ParamDecl>>(params),
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
  
  std::vector<AST::StatementPtr> statements{};
  while (!is_end() && peek().type != TOK_RIGHT_CURLY) {
      statements.push_back(handle_statement());
      move_cursor();
  }

  assertion(peek().type == TOK_RIGHT_CURLY, "scope must end with a right curly brace");
  return AST::Scope(
      std::forward<std::vector<AST::StatementPtr>>(statements));
}

AST::StatementPtr Parser::handle_statement() {
  AST::StatementPtr result;
  if (peek().type == TOK_VAR) {
    result = handle_vd();
    move_cursor();
    assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
    return result; 
  } 

  if (peek().type == TOK_RETURN) {
    result = handle_return();
    move_cursor();
    assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
    return result; 
  }

  if (peek().type == TOK_IF) {
    result = handle_conditional();
    return result;
  }

  if (peek().type == TOK_FOR) {
    result = handle_for();
    return result;
  }

  auto expr = handle_expr();
  result = AST::StatExpr::make(std::forward<AST::ExpressionPtr>(expr));
  move_cursor();
  assertion(peek().type == TOK_SEMICOLON, "statements must end with a semicolon");
  return result; 
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

AST::StatementPtr Parser::handle_for() {
  assertion(peek().type == TOK_FOR, "for loop must start with for");
  move_cursor();
  std::optional<AST::StatementPtr> var_decl;
  std::optional<AST::ExpressionPtr> cond;
  std::optional<AST::ExpressionPtr> iter;

  if (peek().type != TOK_SEMICOLON) {
    var_decl = handle_vd();
    move_cursor();
  }
  assertion(peek().type == TOK_SEMICOLON, "missing semicolon");
  move_cursor();

  if (peek().type != TOK_SEMICOLON) {
    cond = handle_expr();
    move_cursor();
  }
  assertion(peek().type == TOK_SEMICOLON, "missing semicolon");
  move_cursor();

  if (peek().type != TOK_LEFT_CURLY) {
    iter = handle_expr();
    move_cursor();
  }

  auto body = handle_scope();
  return AST::ForLoop::make(
      std::forward<AST::Scope>(body), 
      std::forward<std::optional<AST::StatementPtr>>(var_decl), 
      std::forward<std::optional<AST::ExpressionPtr>>(cond), 
      std::forward<std::optional<AST::ExpressionPtr>>(iter));
}

AST::StatementPtr Parser::handle_return() {
  assertion(peek().type == TOK_RETURN, "return must start with 'return'");
  if (peek(1).type == TOK_SEMICOLON)
    return AST::Return::make();
  move_cursor();
  auto expr = handle_expr();
  return AST::Return::make(std::forward<AST::ExpressionPtr>(expr));
}

AST::StatementPtr Parser::handle_vd() {
  assertion(peek().type == TOK_VAR, "variable declaration must start with 'var'");
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "variable declaration must have a type");
  auto type = peek();
  move_cursor();
  assertion(peek().type == TOK_IDENTIFIER, "variable name must be a valid identifier");
  auto id = peek();
  move_cursor();
  assertion(peek().type == TOK_EQUAL, "variable is uninitialized");
  move_cursor(); 
  auto expr = handle_expr();
  return AST::VarDecl::make(
      type,
      id, 
      std::forward<AST::ExpressionPtr>(expr));
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
  auto result = assignment();
  if (result)
    return result;

  throw_exception("failed to parse expression");
}

AST::ExpressionPtr Parser::assignment() noexcept {
  auto expr = disjunction();
  if (match({TOK_EQUAL})) { 
    if (!std::holds_alternative<AST::Variable>(*expr))
      return nullptr;
    auto op = peek();
    move_cursor();
    auto right = assignment(); // makes this be read from right to left
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }
  return expr;
}

AST::ExpressionPtr Parser::disjunction() noexcept {
  auto expr = conjunction();

  while (match({TOK_OR})) {
    auto op = peek();
    move_cursor();
    auto right = conjunction();
    expr = AST::BinOperator::make(
        std::forward<AST::ExpressionPtr>(expr),
        op,
        std::forward<AST::ExpressionPtr>(right));
  }

  return expr;
}

AST::ExpressionPtr Parser::conjunction() noexcept {
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

AST::ExpressionPtr Parser::equality() noexcept {
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

AST::ExpressionPtr Parser::comparison() noexcept {
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

AST::ExpressionPtr Parser::term() noexcept {
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

AST::ExpressionPtr Parser::factor() noexcept {
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

AST::ExpressionPtr Parser::unary() noexcept {
  if (match({TOK_MINUS, TOK_BANG}, 0)) {
    auto op = peek(-1);
    auto right = unary();
    return AST::UnOperator::make(
        op,
        std::forward<AST::ExpressionPtr>(right));
  }
  return primary();
}

AST::ExpressionPtr Parser::primary() noexcept {
  auto tok = peek();
  if (tok.type == TOK_NUMBER ||
      tok.type == TOK_STRING || 
      tok.type == TOK_BOOL) {
    return AST::Literal::make(tok);
  }

  if (tok.type == TOK_IDENTIFIER) {
    if (peek(1).type != TOK_LEFT_PAREN)
      return AST::Variable::make(tok);
    move_cursor(2); // Skipping over parenthesis
    std::vector<AST::ExpressionPtr> args{};
    // TODO: Add comma as separators
    while (!is_end()) {
      if (peek().type == TOK_RIGHT_PAREN)
        break;

      args.push_back(handle_expr());
      move_cursor();

      if (peek().type == TOK_RIGHT_PAREN)
        break;
      assertion(peek().type == TOK_COMMA, "missing comma");
      move_cursor();
    }
    assertion(peek().type == TOK_RIGHT_PAREN, "missing right parenthesis");
    return AST::FnCall::make(
        tok,
        std::forward<std::vector<AST::ExpressionPtr>>(args));
  }
  if (tok.type == TOK_LEFT_PAREN) {
    move_cursor();
    auto expr = handle_expr();
    move_cursor();
    assertion(peek().type == TOK_RIGHT_PAREN, "missing right parenthesis");
    return AST::Grouping::make(
        std::forward<AST::ExpressionPtr>(expr));
  }
  return nullptr;
}
