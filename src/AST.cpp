#include "AST.hpp"

llvm::BasicBlock* ScopeAST::codegen(Interpreter* interp) {
  llvm::BasicBlock *block = 
    llvm::BasicBlock::Create(
        *interp -> the_context,
        "scope_entry");

  interp -> the_builder -> SetInsertPoint(block);

  interp -> add_scope(this);

  for (auto& stat : statements) {
    stat -> codegen(interp);
  }

  interp -> pop_scope();

  return block;
}

void VariableDeclarationAST::codegen(Interpreter* interp) {
  if (opt_expression)
    interp -> set_named_value(id, opt_expression -> codegen(interp));
  else interp -> set_named_value(id, nullptr);
}

void SExpressionAST::codegen(Interpreter* interp) {
  expr -> codegen(interp);
}

llvm::Value* LiteralAST::codegen(Interpreter* interp) {
  if (std::holds_alternative<std::uint32_t>(value.literal)) {
    return interp -> the_builder -> 
      getInt32(std::get<std::uint32_t>(value.literal)); // TODO: move this to int64 
  }
  if (std::holds_alternative<std::string>(value.literal)) {
    return interp -> the_builder ->
      CreateGlobalStringPtr(
          std::get<std::string>(value.literal),
          "strtmp",
          0,
          interp -> get_module());
  }
  throw interpreter_exception(value, "not implemented yet");
  return nullptr;
};

llvm::Value* VariableAST::codegen(Interpreter* interp) {
  return interp -> get_named_value(id);
}

llvm::Value* BinaryOperationAST::codegen(Interpreter* interp) {
  llvm::Value* lhs_eval = lhs -> codegen(interp);
  llvm::Value* rhs_eval = rhs -> codegen(interp);

  auto* builder = interp -> the_builder.get();
  
  switch (op.type) {
    break; case TOK_PLUS:
      return builder -> CreateAdd(lhs_eval, rhs_eval, "addtmp");
    break; case TOK_MINUS:
      return builder -> CreateSub(lhs_eval, rhs_eval, "subtmp");
    break; case TOK_STAR:
      return builder -> CreateMul(lhs_eval, rhs_eval, "multmp");
    break; case TOK_SLASH:
      return builder -> CreateSDiv(lhs_eval, rhs_eval, "divtmp");
    break; case TOK_OR:
      return builder -> CreateOr(lhs_eval, rhs_eval, "ortmp");
    break; case TOK_AND:
      return builder -> CreateAnd(lhs_eval, rhs_eval, "andtmp");
    break; case TOK_LESS:
      return builder -> CreateICmpSLT(lhs_eval, rhs_eval, "lesstmp");
    break; case TOK_LESS_EQUAL:
      return builder -> CreateICmpSLE(lhs_eval, rhs_eval, "lesseqtmp");
    break; case TOK_GREATER:
      return builder -> CreateICmpSGT(lhs_eval, rhs_eval, "greattmp");
    break; case TOK_GREATER_EQUAL:
      return builder -> CreateICmpSGE(lhs_eval, rhs_eval, "greateqtmp");
    break; case TOK_BANG_EQUAL:
      return builder -> CreateICmpNE(lhs_eval, rhs_eval, "noteqtmp");
    break; case TOK_EQUAL_EQUAL:
      return builder -> CreateICmpEQ(lhs_eval, rhs_eval, "eqtmp");
    break; default: 
      throw std::runtime_error(fmt::format("Unknown binary operator '{}'", op.lexeme));
      return nullptr;
  }
}

llvm::Value* UnaryOperationAST::codegen(Interpreter* interp) {
  llvm::Value* rhs_eval = rhs -> codegen(interp);
  
  // TODO: add for different types
  // e.g. CreateFNeg for float
  switch (op.type) {
    break; case TOK_MINUS:
      return interp -> the_builder -> 
        CreateNeg(rhs_eval, "negtmp");
    break; case TOK_BANG:
      return interp -> the_builder -> 
        CreateNot(rhs_eval, "nottmp");
    break; default: 
      throw std::runtime_error(fmt::format("Unknown unary operator '{}'", op.lexeme));
      return nullptr;
  }
}

llvm::Value* GroupingAST::codegen(Interpreter* interp) {
  return expr -> codegen(interp);
}

llvm::Value* FunctionCallAST::codegen(Interpreter* interp) {

  llvm::Function* func = interp -> get_function(id);
  
  std::vector<llvm::Value*> arg_vals;

  arg_vals.reserve(args.size());
  for (auto& arg : args) {
    arg_vals.push_back(arg -> codegen(interp));
  }

  return interp -> the_builder -> CreateCall(func, arg_vals, "calltmp");
}

llvm::Function* FunctionPrototypeAST::codegen(Interpreter* interp) {
  // setting parameter types
  std::vector<llvm::Type*> parameter_types{};
  parameter_types.reserve(parameters.size());
  for (auto& param : parameters) 
    parameter_types.push_back(interp -> get_type(param -> type));

  // return type
  llvm::Type* rt = interp -> get_type(return_type);

  // function signature
  llvm::FunctionType* func_type = 
    llvm::FunctionType::get(
        rt,
        parameter_types,
        false);
  
  // function
  llvm::Function* func = 
    llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        id.lexeme,
        interp -> get_module());

  return func;
}

llvm::Function* ExternFunctionAST::codegen(Interpreter* interp) {
  auto func = proto -> codegen(interp);
  return func;
}

llvm::Function* FunctionDeclarationAST::codegen(Interpreter* interp) {
  auto func = proto -> codegen(interp);

  for (size_t i = 0; i < proto -> parameters.size(); ++i){
    body -> named_values[proto -> parameters[i] -> id.lexeme] = func -> getArg(i);
  }

  auto scope = body -> codegen(interp);
 
  scope -> insertInto(func);

  return func;
}

std::unique_ptr<llvm::Module>&& ModuleAST::codegen(Interpreter* interp) {
  the_module = std::make_unique<llvm::Module>(name.lexeme, *interp -> the_context);
  for (auto& func : functions) {
    func -> codegen(interp);
  }
  interp -> optimize_module(the_module.get());
  return std::move(the_module);
}

void ReturnAST::codegen(Interpreter* interp) {
  if (opt_expression) {
    interp -> the_builder -> CreateRet(opt_expression -> codegen(interp));
  } else {
    interp -> the_builder -> CreateRetVoid();
  }
}
