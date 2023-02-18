#include "AST.hpp"

void ScopeAST::codegen() {
}

void VariableDeclarationAST::codegen() {

}

void SExpressionAST::codegen() {
  expr -> codegen();
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

llvm::Function* FunctionPrototypeAST::codegen() {
  return nullptr;
}

llvm::Function* FunctionDeclarationAST::codegen() {
  return nullptr;
}

llvm::Module* ModuleAST::codegen() {
  return nullptr;
}

void ReturnAST::codegen() {
}
