#include "AST.hpp"

void ScopeAST::codegen(Interpreter* interp) {

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
  return nullptr;
}

llvm::Function* FunctionDeclarationAST::codegen(Interpreter* interp) {
  return nullptr;
}

std::unique_ptr<llvm::Module>&& ModuleAST::codegen(Interpreter* interp) {
  return nullptr;
}

void ReturnAST::codegen(Interpreter* interp) {

}
