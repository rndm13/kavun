#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "lexer.hpp"

// Scope = '{' Statement* '}' 
// 
// Statement = (VariableDeclaration | Expression | Return) ';' 
//
// Return = 'return' Expression
//
// a*  = a a*
//     |  
// 
// VariableDeclaration = 'var' Identifier ('=' Expression)? 
//
// Expression = Literal 
//            | Variable
//            | BinaryOperation
//            | FunctionCall
//            | UnaryOperation
//            | Grouping
//
// FunctionPrototype   = 'fn' Identifier '(' VariableDeclaration* ')'
//
// FunctionDeclaration = FunctionPrototype Scope
// 
// Literal = Number 
//         | String 
//         | Bool 
//         | Null
// 
// BinaryOperation = Expression BinaryOperator Expression 
//
// BinaryOperator  = '+' | '-' | '/'  | '*'  | '='  | 'and' | 'or'
//                 | '>' | '<' | '>=' | '<=' | '=='
//
// FunctionCall = Identifier '(' Expression* ')'
// 
// UnaryOperation = UnaryOperator Expression 
// UnaryOperator = '-'
//  
// Grouping = '(' Expression ')' 
//
// Module = 'module' Name ';'
//          FunctionDeclaration*

// std::unique_ptr<LLVMContext> TheContext;
// std::unique_ptr<Module> TheModule;
// std::unique_ptr<IRBuilder<>> Builder;
// std::map<std::string, Value *> NamedValues;
// std::unique_ptr<legacy::FunctionPassManager> TheFPM;
// std::unique_ptr<KaleidoscopeJIT> TheJIT;
// std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

struct StatementAST {
  typedef std::unique_ptr<StatementAST> Ptr;
  virtual ~StatementAST() {};
  virtual void codegen() = 0;
};

struct ScopeAST {
  std::vector<StatementAST::Ptr> statements;
  ScopeAST(std::vector<StatementAST::Ptr>&& input) : statements(input.size()) { 
    for (size_t ind = 0;ind < input.size(); ++ind) {
      statements[ind].reset(input[ind].release());
    }
  } 

  void codegen(); 

  typedef std::unique_ptr<ScopeAST> Ptr;
};

struct ExpressionAST {
  typedef std::unique_ptr<ExpressionAST> Ptr; 
  
  virtual ~ExpressionAST() {};
  virtual llvm::Value* codegen() = 0;
};

// Statement Expression not lisp's "s-expression"
struct SExpressionAST : StatementAST { 
  typedef std::unique_ptr<SExpressionAST> Ptr;
  ExpressionAST::Ptr expr;
  SExpressionAST(ExpressionAST::Ptr&& _expr) 
    : expr(std::forward<ExpressionAST::Ptr>(_expr)) { }

  void codegen() override;
};

struct VariableDeclarationAST : StatementAST {
  typedef std::unique_ptr<VariableDeclarationAST> Ptr;
  Token id;
  ExpressionAST::Ptr opt_expression{nullptr};
  Token type;

  VariableDeclarationAST(Token _type, Token _id, ExpressionAST::Ptr&& _expr) : id(_id), type(_type) {
    opt_expression.reset(_expr.release());
  }

  void codegen() override; 
};

struct LiteralAST : ExpressionAST {
  typedef std::unique_ptr<LiteralAST> Ptr;
  Token value;
  LiteralAST(const Token& t) : value(t) { }

  llvm::Value* codegen() override; 
};

struct VariableAST : ExpressionAST {
  typedef std::unique_ptr<VariableAST> Ptr;
  Token id;
  VariableAST(const Token& t) : id(t) { }

  llvm::Value* codegen() override; 
};

struct BinaryOperationAST : ExpressionAST {
  typedef std::unique_ptr<BinaryOperationAST> Ptr;
  ExpressionAST::Ptr lhs, rhs;
  Token op;

  BinaryOperationAST(ExpressionAST::Ptr&& _lhs, const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    lhs.reset(_lhs.release());
    rhs.reset(_rhs.release());
  }

  llvm::Value* codegen() override; 
};

struct UnaryOperationAST : ExpressionAST {
  typedef std::unique_ptr<UnaryOperationAST> Ptr;
  Token op;
  ExpressionAST::Ptr rhs;
  UnaryOperationAST(const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    rhs.reset(_rhs.release());
  }

  llvm::Value* codegen() override; 
};

struct GroupingAST : ExpressionAST {
  typedef std::unique_ptr<GroupingAST> Ptr;
  ExpressionAST::Ptr expr;

  GroupingAST(ExpressionAST::Ptr&& _expr) {
    expr.reset(_expr.release());
  }

  llvm::Value* codegen() override; 
};

struct FunctionCallAST : ExpressionAST {
  typedef std::unique_ptr<FunctionCallAST> Ptr;

  Token id;
  std::vector<ExpressionAST::Ptr> args;
  FunctionCallAST(const Token& _id, std::vector<ExpressionAST::Ptr>&& input) : id(_id), args(input.size()) { 
    for (size_t ind = 0; ind < input.size(); ++ind) {
      args[ind].reset(input[ind].release());
    }
  }

  llvm::Value* codegen() override; 
};

struct ReturnAST : StatementAST {
  typedef std::unique_ptr<ReturnAST> Ptr;
  ExpressionAST::Ptr opt_expression;
  ReturnAST(ExpressionAST::Ptr&& _expression) 
    : opt_expression(std::forward<ExpressionAST::Ptr>(_expression)) { }

  void codegen() override;
};

struct FunctionPrototypeAST {
  typedef std::unique_ptr<FunctionPrototypeAST> Ptr;

  Token id;
  std::vector<VariableDeclarationAST::Ptr> parameters;
  Token return_type;

  FunctionPrototypeAST(const Token& _id, std::vector<VariableDeclarationAST::Ptr>&& _params, const Token& _return_type) 
    : id (_id), parameters(_params.size()), return_type(_return_type) {
    for (size_t ind = 0; ind < _params.size(); ++ind) {
      parameters[ind].reset(_params[ind].release());
    }
  }

  llvm::Function* codegen(); 
};

struct FunctionDeclarationAST {
  typedef std::unique_ptr<FunctionDeclarationAST> Ptr;

  FunctionPrototypeAST::Ptr proto;
  ScopeAST::Ptr body;
  FunctionDeclarationAST(FunctionPrototypeAST::Ptr&& _proto, ScopeAST::Ptr&& _body) {
    proto.reset(_proto.release());
    body.reset(_body.release());
  }

  llvm::Function* codegen(); 
};

struct ModuleAST {
  typedef std::unique_ptr<ModuleAST> Ptr;
  Token name;
  std::vector<FunctionDeclarationAST::Ptr> functions;

  ModuleAST(const Token& _name, std::vector<FunctionDeclarationAST::Ptr>&& funcs) : name(_name), functions(funcs.size()) {
    for (size_t ind = 0; ind < funcs.size(); ++ind) {
      functions.at(ind).reset(funcs.at(ind).release());
    }
  }

  llvm::Module* codegen(); 
};

