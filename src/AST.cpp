#include "AST.hpp"

llvm::BasicBlock* ScopeAST::codegen(Interpreter* interp) {
  llvm::BasicBlock *block = 
    llvm::BasicBlock::Create(
        *interp -> the_context,
        "scope_entry");

  interp -> the_builder -> SetInsertPoint(block);

  for (auto& stat : statements) {
    stat -> codegen(interp);
  }

  return block;
}

void VariableDeclarationAST::codegen(Interpreter* interp) {
}

void SExpressionAST::codegen(Interpreter* interp) {
  expr -> codegen(interp);
}

llvm::Value* LiteralAST::codegen(Interpreter* interp) {
  if (std::holds_alternative<std::uint32_t>(value.literal)) {
    return interp -> the_builder -> 
      getInt32(std::get<std::uint32_t>(value.literal)); // TODO: move this to int64 
  } 
  throw interpreter_exception(value, "not implemented yet");
  return nullptr;
};

llvm::Value* VariableAST::codegen(Interpreter* interp) {
  return nullptr;
}

llvm::Value* BinaryOperationAST::codegen(Interpreter* interp) {
  return nullptr;
}

llvm::Value* UnaryOperationAST::codegen(Interpreter* interp) {
  return nullptr;
}

llvm::Value* GroupingAST::codegen(Interpreter* interp) {
  return expr -> codegen(interp);
}

llvm::Value* FunctionCallAST::codegen(Interpreter* interp) {
  return nullptr;
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
  return std::move(the_module);
}

void ReturnAST::codegen(Interpreter* interp) {
  if (opt_expression) {
    interp -> the_builder -> CreateRet(opt_expression -> codegen(interp));
  } else {
    interp -> the_builder -> CreateRetVoid();
  }
}
