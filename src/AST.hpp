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
// Statement = (VariableDeclaration | Expression) ';' 
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
// Program = FunctionDeclaration*

// TODO: Move all ASTs to seperate file

struct AST { 
  typedef std::unique_ptr<AST> Ptr;
  virtual std::string pretty_show() const = 0;
  virtual llvm::Value* codegen() = 0;
  virtual ~AST() { }

  protected:
//   static std::unique_ptr<LLVMContext> TheContext;
//   static std::unique_ptr<Module> TheModule;
//   static std::unique_ptr<IRBuilder<>> Builder;
//   static std::map<std::string, Value *> NamedValues;
//   static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
//   static std::unique_ptr<KaleidoscopeJIT> TheJIT;
//   static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
};

struct StatementAST : AST {
  typedef std::unique_ptr<StatementAST> Ptr;

};

struct ScopeAST : AST {
  std::vector<StatementAST::Ptr> statements;
  ScopeAST(std::vector<StatementAST::Ptr>&& input) : statements(input.size()) { 
    for (size_t ind = 0;ind < input.size(); ++ind) {
      statements[ind].reset(input[ind].release());
    }
  } 

  std::string pretty_show() const {
    std::string result = "ScopeAST {\n";
    for (auto& st : statements) {
      result += st -> pretty_show() + "\n";
    }
    result += "}\n";
    return result;
  }

  llvm::Value* codegen() override; 

  typedef std::unique_ptr<ScopeAST> Ptr;
};

struct ExpressionAST : StatementAST {
  typedef std::unique_ptr<ExpressionAST> Ptr; 

};

struct VariableDeclarationAST : StatementAST {
  Token id;
  ExpressionAST::Ptr opt_expression{nullptr};

  VariableDeclarationAST(Token _id, ExpressionAST::Ptr&& _expr) : id(_id) {
    opt_expression.reset(_expr.release());
  }

  std::string pretty_show() const {
    if (opt_expression)
      return fmt::format("VariableDeclarationAST {} = {}", id.lexeme, opt_expression -> pretty_show());
    return fmt::format("VariableDeclarationAST {}", id.lexeme);
  }

  llvm::Value* codegen() override; 
};

struct LiteralAST : ExpressionAST {
  Token value;
  LiteralAST(const Token& t) : value(t) { }

  std::string pretty_show() const {
    return fmt::format("LiteralAST {}", value.lexeme);
  }

  llvm::Value* codegen() override; 
};

struct VariableAST : ExpressionAST {
  Token id;
  VariableAST(const Token& t) : id(t) { }

  std::string pretty_show() const {
    return fmt::format("VariableAST {}", id.lexeme);
  }

  llvm::Value* codegen() override; 
};

struct BinaryOperationAST : ExpressionAST {
  ExpressionAST::Ptr lhs, rhs;
  Token op;

  BinaryOperationAST(ExpressionAST::Ptr&& _lhs, const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    lhs.reset(_lhs.release());
    rhs.reset(_rhs.release());
  }

  std::string pretty_show() const {
    // return fmt::format("BinaryOperationAST {} {} {}", lhs -> pretty_show(), op.lexeme, rhs -> pretty_show());
    return fmt::format("({} {} {})", lhs -> pretty_show(), op.lexeme, rhs -> pretty_show());
  }

  llvm::Value* codegen() override; 
};

struct UnaryOperationAST : ExpressionAST {
  Token op;
  ExpressionAST::Ptr rhs;
  UnaryOperationAST(const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    rhs.reset(_rhs.release());
  }

  std::string pretty_show() const {
    // return fmt::format("UnaryOperationAST {} {}", op.lexeme, rhs -> pretty_show());
    return fmt::format("({} {})", op.lexeme, rhs -> pretty_show());
  }

  llvm::Value* codegen() override; 
};

struct GroupingAST : ExpressionAST {
  ExpressionAST::Ptr expr;

  GroupingAST(ExpressionAST::Ptr&& _expr) {
    expr.reset(_expr.release());
  }

  std::string pretty_show() const {
    return fmt::format("GroupingAST ({})", expr -> pretty_show());
  }

  llvm::Value* codegen() override; 
};

struct FunctionCallAST : ExpressionAST {
  Token id;
  std::vector<ExpressionAST::Ptr> args;
  FunctionCallAST(const Token& _id, std::vector<ExpressionAST::Ptr>&& input) : id(_id), args(input.size()) { 
    for (size_t ind = 0; ind < input.size(); ++ind) {
      args[ind].reset(input[ind].release());
    }
  }

  std::string pretty_show() const {
    std::vector<std::string> args_str(args.size());
    for (size_t ind = 0; ind < args.size(); ++ind) {
      args_str.at(ind) = args.at(ind) -> pretty_show();
    }
    return fmt::format("FunctionCallAST {} ({})", id.lexeme, fmt::join(args_str, " "));
  }

  llvm::Value* codegen() override; 
};

struct FunctionPrototypeAST : AST {
  Token id;
  std::vector<VariableDeclarationAST::Ptr> parameters;

  FunctionPrototypeAST(const Token& _id, std::vector<VariableDeclarationAST::Ptr>&& _params) 
    : id (_id), parameters(_params.size()) {
    for (size_t ind = 0; ind < _params.size(); ++ind) {
      parameters[ind].reset(_params[ind].release());
    }
  }

  llvm::Value* codegen() override; 

  std::string pretty_show() const {
    std::vector<std::string> params(parameters.size());
    for (size_t ind = 0; ind < parameters.size(); ++ind) {
      params.at(ind) = parameters.at(ind)->pretty_show();
    }
    return fmt::format("FunctionPrototypeAST {} ({})", id.lexeme, fmt::join(params, "\n"));
  }
  typedef std::unique_ptr<FunctionPrototypeAST> Ptr;
};

struct FunctionDeclarationAST : AST {
  FunctionPrototypeAST::Ptr proto;
  ScopeAST::Ptr body;
  FunctionDeclarationAST(FunctionPrototypeAST::Ptr&& _proto, ScopeAST::Ptr&& _body) {
    proto.reset(_proto.release());
    body.reset(_body.release());
  }

  std::string pretty_show() const {
    return fmt::format("FunctionDeclarationAST {} {}", proto -> pretty_show(), body -> pretty_show());
  }

  llvm::Value* codegen() override; 
};

struct ModuleAST : AST {
  std::vector<FunctionDeclarationAST::Ptr> functions;

  ModuleAST(std::vector<FunctionDeclarationAST::Ptr>&& funcs) : functions(funcs.size()) {
    for (size_t ind = 0; ind < funcs.size(); ++ind) {
      functions.at(ind).reset(funcs.at(ind).release());
    }
  }

  std::string pretty_show() const {
    std::vector<std::string> func_str(functions.size());
    for (size_t ind = 0; ind < functions.size(); ++ind) {
      func_str.at(ind) = functions.at(ind) -> pretty_show();
    }
    return fmt::format("{}", fmt::join(func_str, "\n")); 
  }

  llvm::Value* codegen() override; 

  typedef std::unique_ptr<ModuleAST> Ptr; 
};

