#include "AST.hpp"

llvm::BasicBlock* ScopeAST::codegen(Interpreter* interp) {
  auto builder = interp -> get_builder();
  llvm::BasicBlock *block = 
    llvm::BasicBlock::Create(
        *interp -> the_context,
        "scope_entry");
  return block;
}

void VariableDeclarationAST::codegen(Interpreter* interp) {
}

void SExpressionAST::codegen(Interpreter* interp) {
  expr -> codegen(interp);
}

llvm::Value* LiteralAST::codegen(Interpreter* interp) {
  return nullptr;
}

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
  
}

std::unique_ptr<llvm::Module>&& ModuleAST::codegen(Interpreter* interp) {
  the_module = std::make_unique<llvm::Module>(name.lexeme, *interp -> the_context);
  return std::move(the_module);
}

void ReturnAST::codegen(Interpreter* interp) {

}
