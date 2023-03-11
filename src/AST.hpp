#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>
#include <stack>
#include <optional>
#include <variant>

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
#include "llvm/IR/PassInstrumentation.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/ModuleSummaryAnalysis.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
// #include "llvm/MC/TargetRegistry.h"
// #include "llvm/TargetParser/Host.h"

#include "lexer.hpp"

namespace AST {
struct Module;

struct FnProto;
struct Scope;

// Top level
struct Extern;
struct FnDecl;

// Statements
struct Conditional;
struct StatExpr;
struct Return;
struct VarDecl;

// Expression
struct BinOperator;
struct UnOperator;
struct Literal;
struct Variable;
struct Grouping;
struct FnCall;

using TopLevel = std::variant
  <Extern, FnDecl>;
using TopLevelPtr = std::unique_ptr<TopLevel>;

using Statement = std::variant
  <Conditional, StatExpr, Return, VarDecl>;
using StatementPtr = std::unique_ptr<Statement>;
  
using Expression = std::variant
  <BinOperator, UnOperator, Literal, Variable, Grouping, FnCall>;
using ExpressionPtr = std::unique_ptr<Expression>;

struct FnProto {
  Token id;
  std::vector<VarDecl> parameters;
  Token return_type;

  FnProto(const Token& _id, std::vector<VarDecl>&& _params, const Token& _return_type);
};

struct Scope {
  std::vector<StatementPtr> statements;
  Scope(std::vector<StatementPtr>&&);

  std::map<
    std::string,
    std::unique_ptr<llvm::Value, 
    decltype([](llvm::Value* ptr) {
      ptr -> deleteValue();
    })>> named_values{};
};

// Statement Expression not lisp's "s-expression"
struct StatExpr { 
  ExpressionPtr expr;
  StatExpr(ExpressionPtr&&);
  static StatementPtr make(ExpressionPtr&&);
};

struct Return {
  std::optional<ExpressionPtr> opt_expression;
  Return(std::optional<ExpressionPtr>&& _expression = std::nullopt);
  static StatementPtr make(std::optional<ExpressionPtr>&& _expression = std::nullopt);
};

struct VarDecl {
  Token type;
  Token id;
  ExpressionPtr opt_expression{nullptr};

  VarDecl(const Token& _type, const Token& _id, ExpressionPtr&& _expr);
  static StatementPtr make(const Token&, const Token&, ExpressionPtr&&);
};

struct Literal {
  Token value;
  Literal(const Token& t);

  static ExpressionPtr make(const Token&);
};

struct Variable {
  Token id;
  Variable(const Token& t);

  static ExpressionPtr make(const Token&);
};

struct BinOperator{
  Token op;
  ExpressionPtr lhs, rhs;

  BinOperator(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs);
  static ExpressionPtr make(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs);
};

struct UnOperator {
  Token op;
  ExpressionPtr  rhs;
  UnOperator(const Token& t, ExpressionPtr && _rhs) ;
  static ExpressionPtr make(const Token& t, ExpressionPtr&& _rhs);
};

struct Grouping {
  ExpressionPtr  expr;

  Grouping(ExpressionPtr&& _expr);
  static ExpressionPtr make(ExpressionPtr&& _expr);
};

struct FnCall {
  Token id;
  std::vector<ExpressionPtr> args;
  FnCall(const Token& _id, std::vector<ExpressionPtr>&& input);
  static ExpressionPtr make(const Token& _id, std::vector<ExpressionPtr>&& _args);
};

struct Extern {
  FnProto proto;
  Extern(FnProto&& _proto);
  static TopLevelPtr make(FnProto&& _proto);
};

struct FnDecl {
  FnProto proto;
  Scope body;
  FnDecl(FnProto&& _proto, Scope&& _body);
  static TopLevelPtr make(FnProto&& _proto, Scope&& _body);
};

struct Conditional {
  Token id;
  ExpressionPtr condition;

  Scope if_body;
  std::optional<Scope> else_body;

  Conditional(
      const Token& _id,
      ExpressionPtr&& _cond,
      Scope&& _if,
      std::optional<Scope>&& _else = std::nullopt);

  static StatementPtr make(
      const Token& _id,
      ExpressionPtr&& _cond,
      Scope&& _if,
      std::optional<Scope>&& _else = std::nullopt);
};

struct Module {
  Token name;
  std::vector<TopLevelPtr> functions;

  Module(const Token& _name, std::vector<TopLevelPtr>&& funcs);

  std::unique_ptr<llvm::Module> the_module;
};

} // AST namespace

// class interpreter_exception : std::exception {
//   std::string info;
// public:
//   interpreter_exception(const Token& tok, const std::string& in)
//   : info(fmt::format("('{}', line {}, col {}) : {}", tok.lexeme, tok.line, tok.col, in)) { }
//   virtual const char* what() {
//     return info.c_str();
//   }
// };
// 
// class Interpreter {
// public:
//   std::unique_ptr<llvm::LLVMContext> the_context;
//   std::unique_ptr<llvm::IRBuilder<>> the_builder;
// 
//   // TODO: add global scope
//   std::stack<ScopeAST*> scope_stack;
// 
//   std::map<std::string, llvm::Type*> type_lookup;
//   
//   ModuleAST::Ptr current_module; llvm::Function* get_function(Token identifier) {
//     auto result = get_module() -> getFunction(identifier.lexeme);
//     if (result)
//       return result;
//     throw interpreter_exception(identifier, "function not found");
//     return nullptr;
//   }
// 
//   llvm::Module* get_module() {
//     return current_module -> the_module.get();
//   }
// 
//   llvm::IRBuilder<>* get_builder() {
//     return the_builder.get();
//   }
// 
//   llvm::Value* get_named_value(Token identifier) {
//     try {
//       if (scope_stack.empty()) {
//         throw interpreter_exception(identifier, "global scope is currently not implemented");
//         return nullptr;
//       }
//       return scope_stack.top() -> named_values.at(identifier.lexeme);
//     } catch (std::out_of_range& e) {
//       throw interpreter_exception(identifier, "named value not found");
//       return nullptr;
//     }
//   }
// 
//   void set_named_value(Token identifier, llvm::Value* val) {
//     if (scope_stack.empty()) {
//       throw interpreter_exception(identifier, "global scope is currently not implemented");
//       return;
//     }
// 
//     scope_stack.top() -> named_values[identifier.lexeme] = val;
//   }
// 
//   void add_scope(ScopeAST* scope) {
//     if (!scope_stack.empty())
//       scope -> named_values = scope_stack.top() -> named_values;
// 
//     scope_stack.push(scope);
//   }
// 
//   void pop_scope() {
//     if (scope_stack.empty()) {
//       throw std::runtime_error("cannot pop empty scope stack"); 
//     } else {
//       scope_stack.pop();
//     }
//   }
// 
//   llvm::Type* get_type(Token type) {
//     try {
//       return type_lookup.at(type.lexeme);
//     } catch (std::out_of_range& e) {
//       throw interpreter_exception(type, "no such type found");
//     }
//   }
// 
//   Interpreter(ModuleAST::Ptr&& _module) 
//     : current_module(std::forward<ModuleAST::Ptr>(_module)) {
//     the_context = std::make_unique<llvm::LLVMContext>();
//     the_builder = std::make_unique<llvm::IRBuilder<>>(*the_context);
//     type_lookup["i32"]  = llvm::Type::getInt32Ty(*the_context);
//     type_lookup["bool"]  = llvm::Type::getInt1Ty(*the_context);
//     type_lookup["void"] = llvm::Type::getVoidTy(*the_context);
//     // TODO: change string to a class
//     type_lookup["string"] = 
//       llvm::PointerType::get(llvm::Type::getInt8Ty(*the_context), 0); 
//     // TODO: add double
//   }
// 
//   void optimize_module(llvm::Module*);
// 
//   // void print_mc(llvm::Module* module_ptr, std::string file_name = "output.o") {
//   //   auto target_triple = llvm::sys::getDefaultTargetTriple();
//   //   llvm::InitializeAllTargetInfos();
//   //   llvm::InitializeAllTargets();
//   //   llvm::InitializeAllTargetMCs();
//   //   llvm::InitializeAllAsmParsers();
//   //   llvm::InitializeAllAsmPrinters();
// 
//   //   std::string error = "failed to find requested target";
//   //   auto target = llvm::TargetRegistry::lookupTarget(
//   //       target_triple,
//   //       error);
// 
//   //   if (!target) {
//   //     throw std::runtime_error(error);
//   //     return;
//   //   }
// 
//   //   auto CPU = "generic";
//   //   auto Features = "";
// 
//   //   llvm::TargetOptions opt;
//   //   auto RM = std::optional<llvm::Reloc::Model>();
// 
//   //   auto TargetMachine = 
//   //     llvm::Target->createTargetMachine(target_triple, CPU, Features, opt, RM);
// 
//   //   module_ptr -> setDataLayout(TargetMachine->createDataLayout());
//   //   module_ptr -> setTargetTriple(target_triple);
// 
//   //   std::error_code EC;
//   //   llvm::raw_fd_ostream dest(file_name, EC, llvm::sys::fs::OF_None);
// 
//   //   if (EC) {
//   //     throw std::runtime_error(fmt::format("could not open file '{}'", file_name));
//   //     return;
//   //   }
// 
//   //   legacy::PassManager pass; // TODO: change this to smth else
//   //   auto FileType = llvm::CGFT_ObjectFile;
// 
//   //   if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
//   //     throw std::runtime_error("TargetMachine can't emit a file of this type");
//   //     return;
//   //   }
// 
//   //   pass.run(*module_ptr);
//   //   dest.flush();
//   // }
// 
//   std::unique_ptr<llvm::Module>&& run() {
//     return current_module -> codegen(this);
//   }
// };
// 
