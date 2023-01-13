#include "AST.hpp"

llvm::Value* ScopeAST::codegen() {
  return nullptr;
}

llvm::Value* VariableDeclarationAST::codegen() {
  return nullptr;
}

llvm::Value* LiteralAST::codegen() {
  return nullptr;
}

llvm::Value* VariableAST::codegen() {
  return nullptr;
}

llvm::Value* BinaryOperationAST::codegen() {
  return nullptr;
}

llvm::Value* UnaryOperationAST::codegen() {
  return nullptr;
}

llvm::Value* GroupingAST::codegen() {
  return expr -> codegen();
}

llvm::Value* FunctionCallAST::codegen() {
  return nullptr;
}

llvm::Value* FunctionPrototypeAST::codegen() {
  return nullptr;
}

llvm::Value* FunctionDeclarationAST::codegen() {
  return nullptr;
}

llvm::Value* ModuleAST::codegen() {
  return nullptr;
}
