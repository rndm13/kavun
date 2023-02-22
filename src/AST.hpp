#pragma once

#include <vector>
#include <stack>
#include <string>
#include <memory>
#include <functional>
#include <stack>

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

#include "lexer.hpp"

class Interpreter;

struct StatementAST {
  typedef std::unique_ptr<StatementAST> Ptr;
  virtual ~StatementAST() {};
  virtual void codegen(Interpreter*) = 0;
};

struct ScopeAST {
  typedef std::unique_ptr<ScopeAST> Ptr;

  std::vector<StatementAST::Ptr> statements;
  ScopeAST(std::vector<StatementAST::Ptr>&& input) : statements(input.size()) { 
    for (size_t ind = 0;ind < input.size(); ++ind) {
      statements[ind].reset(input[ind].release());
    }
  } 

  llvm::BasicBlock* codegen(Interpreter*); 

  std::map<std::string, llvm::Value*> named_values;
};

struct ExpressionAST {
  typedef std::unique_ptr<ExpressionAST> Ptr; 
  
  virtual ~ExpressionAST() {};
  virtual llvm::Value* codegen(Interpreter*) = 0;
};

// Statement Expression not lisp's "s-expression"
struct SExpressionAST : StatementAST { 
  typedef std::unique_ptr<SExpressionAST> Ptr;
  ExpressionAST::Ptr expr;
  SExpressionAST(ExpressionAST::Ptr&& _expr) 
    : expr(std::forward<ExpressionAST::Ptr>(_expr)) { }

  void codegen(Interpreter*) override;
};

struct VariableDeclarationAST : StatementAST {
  typedef std::unique_ptr<VariableDeclarationAST> Ptr;
  Token id;
  ExpressionAST::Ptr opt_expression{nullptr};
  Token type;

  VariableDeclarationAST(Token _type, Token _id, ExpressionAST::Ptr&& _expr) : id(_id), type(_type) {
    opt_expression.reset(_expr.release());
  }

  void codegen(Interpreter*) override; 
};

struct LiteralAST : ExpressionAST {
  typedef std::unique_ptr<LiteralAST> Ptr;
  Token value;
  LiteralAST(const Token& t) : value(t) { }

  llvm::Value* codegen(Interpreter*) override; 
};

struct VariableAST : ExpressionAST {
  typedef std::unique_ptr<VariableAST> Ptr;
  Token id;
  VariableAST(const Token& t) : id(t) { }

  llvm::Value* codegen(Interpreter*) override; 
};

struct BinaryOperationAST : ExpressionAST {
  typedef std::unique_ptr<BinaryOperationAST> Ptr;
  ExpressionAST::Ptr lhs, rhs;
  Token op;

  BinaryOperationAST(ExpressionAST::Ptr&& _lhs, const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    lhs.reset(_lhs.release());
    rhs.reset(_rhs.release());
  }

  llvm::Value* codegen(Interpreter*) override; 
};

struct UnaryOperationAST : ExpressionAST {
  typedef std::unique_ptr<UnaryOperationAST> Ptr;
  Token op;
  ExpressionAST::Ptr rhs;
  UnaryOperationAST(const Token& t, ExpressionAST::Ptr&& _rhs) : op(t) {
    rhs.reset(_rhs.release());
  }

  llvm::Value* codegen(Interpreter*) override; 
};

struct GroupingAST : ExpressionAST {
  typedef std::unique_ptr<GroupingAST> Ptr;
  ExpressionAST::Ptr expr;

  GroupingAST(ExpressionAST::Ptr&& _expr) {
    expr.reset(_expr.release());
  }

  llvm::Value* codegen(Interpreter*) override; 
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

  llvm::Value* codegen(Interpreter*) override; 
};

struct ReturnAST : StatementAST {
  typedef std::unique_ptr<ReturnAST> Ptr;
  ExpressionAST::Ptr opt_expression;
  ReturnAST(ExpressionAST::Ptr&& _expression) 
    : opt_expression(std::forward<ExpressionAST::Ptr>(_expression)) { }

  void codegen(Interpreter*) override;
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

  llvm::Function* codegen(Interpreter*); 
};

struct TopLevelAST {
  typedef std::unique_ptr<TopLevelAST> Ptr;
  virtual ~TopLevelAST() {}
  virtual llvm::Function* codegen(Interpreter*) = 0;
};

struct ExternFunctionAST : TopLevelAST {
  typedef std::unique_ptr<ExternFunctionAST> Ptr;

  FunctionPrototypeAST::Ptr proto;
  ExternFunctionAST(FunctionPrototypeAST::Ptr&& _proto) 
    : proto(std::forward<FunctionPrototypeAST::Ptr>(_proto)) { }

  llvm::Function* codegen(Interpreter*) override; 
};

struct FunctionDeclarationAST : TopLevelAST {
  typedef std::unique_ptr<FunctionDeclarationAST> Ptr;

  FunctionPrototypeAST::Ptr proto;
  ScopeAST::Ptr body;
  FunctionDeclarationAST(FunctionPrototypeAST::Ptr&& _proto, ScopeAST::Ptr&& _body) {
    proto.reset(_proto.release());
    body.reset(_body.release());
  }

  llvm::Function* codegen(Interpreter*) override; 
};

struct ModuleAST {
  typedef std::unique_ptr<ModuleAST> Ptr;
  Token name;
  std::vector<TopLevelAST::Ptr> functions;

  ModuleAST(const Token& _name, std::vector<TopLevelAST::Ptr>&& funcs) : name(_name), functions(funcs.size()) {
    for (size_t ind = 0; ind < funcs.size(); ++ind) {
      functions.at(ind).reset(funcs.at(ind).release());
    }
  }

  std::unique_ptr<llvm::Module>&& codegen(Interpreter*); 

  std::unique_ptr<llvm::Module> the_module;
};

class interpreter_exception : std::exception {
  std::string info;
public:
  interpreter_exception(const Token& tok, const std::string& in)
  : info(fmt::format("('{}', line {}, col {}) : {}", tok.lexeme, tok.line, tok.col, in)) { }
  virtual const char* what() {
    return info.c_str();
  }
};

class Interpreter {
public:
  std::unique_ptr<llvm::LLVMContext> the_context;
  std::unique_ptr<llvm::IRBuilder<>> the_builder;

  // TODO: add global scope
  std::stack<ScopeAST*> scope_stack;

  std::map<std::string, llvm::Type*> type_lookup;
  
  ModuleAST::Ptr current_module; llvm::Function* get_function(Token identifier) {
    auto result = get_module() -> getFunction(identifier.lexeme);
    if (result)
      return result;
    throw interpreter_exception(identifier, "function not found");
    return nullptr;
  }

  llvm::Module* get_module() {
    return current_module -> the_module.get();
  }

  llvm::IRBuilder<>* get_builder() {
    return the_builder.get();
  }

  llvm::Value* get_named_value(Token identifier) {
    try {
      if (scope_stack.empty()) {
        throw interpreter_exception(identifier, "global scope is currently not implemented");
        return nullptr;
      }
      return scope_stack.top() -> named_values.at(identifier.lexeme);
    } catch (std::out_of_range& e) {
      throw interpreter_exception(identifier, "named value not found");
      return nullptr;
    }
  }

  void set_named_value(Token identifier, llvm::Value* val) {
    if (scope_stack.empty()) {
      throw interpreter_exception(identifier, "global scope is currently not implemented");
      return;
    }

    scope_stack.top() -> named_values[identifier.lexeme] = val;
  }

  void add_scope(ScopeAST* scope) {
    if (!scope_stack.empty())
      scope -> named_values = scope_stack.top() -> named_values;

    scope_stack.push(scope);
  }

  void pop_scope() {
    if (scope_stack.empty()) {
      throw std::runtime_error("cannot pop empty scope stack"); 
    } else {
      scope_stack.pop();
    }
  }

  llvm::Type* get_type(Token type) {
    try {
      return type_lookup.at(type.lexeme);
    } catch (std::out_of_range& e) {
      throw interpreter_exception(type, "no such type found");
    }
  }

  Interpreter(ModuleAST::Ptr&& _module) 
    : current_module(std::forward<ModuleAST::Ptr>(_module)) {
    the_context = std::make_unique<llvm::LLVMContext>();
    the_builder = std::make_unique<llvm::IRBuilder<>>(*the_context);
    type_lookup["i32"]  = llvm::Type::getInt32Ty(*the_context);
    type_lookup["void"] = llvm::Type::getVoidTy(*the_context);
    // TODO: change string to a class
    type_lookup["string"] = 
      llvm::PointerType::get(llvm::Type::getInt8Ty(*the_context), 0); 
    // TODO: add double
  }

  void optimize_module(llvm::Module* module_ptr) {
    // Create the analysis managers.
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    llvm::PassBuilder PB;

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    // Create the pass manager.
    // This one corresponds to a typical -O2 optimization pipeline.
    llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(
        llvm::PassBuilder::OptimizationLevel::O2);

    // Optimize the IR!
    MPM.run(*module_ptr, MAM); 
  }

//   void print_mc(llvm::Module* module_ptr, std::string file_name = "output.o") {
//     auto target_triple = llvm::sys::getDefaultTargetTriple();
//     llvm::InitializeAllTargetInfos();
//     llvm::InitializeAllTargets();
//     llvm::InitializeAllTargetMCs();
//     llvm::InitializeAllAsmParsers();
//     llvm::InitializeAllAsmPrinters();
// 
//     std::string error = "failed to find requested target";
//     auto target = llvm::TargetRegistry::lookupTarget(
//         target_triple,
//         error);
// 
//     if (!target) {
//       throw std::runtime_error(error);
//       return;
//     }
// 
//     auto CPU = "generic";
//     auto Features = "";
// 
//     llvm::TargetOptions opt;
//     auto RM = std::optional<llvm::Reloc::Model>();
// 
//     auto TargetMachine = 
//       llvm::Target->createTargetMachine(target_triple, CPU, Features, opt, RM);
// 
//     module_ptr -> setDataLayout(TargetMachine->createDataLayout());
//     module_ptr -> setTargetTriple(target_triple);
// 
//     std::error_code EC;
//     llvm::raw_fd_ostream dest(file_name, EC, llvm::sys::fs::OF_None);
// 
//     if (EC) {
//       throw std::runtime_error(fmt::format("could not open file '{}'", file_name));
//       return;
//     }
// 
//     legacy::PassManager pass; // TODO: change this to smth else
//     auto FileType = llvm::CGFT_ObjectFile;
// 
//     if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
//       throw std::runtime_error("TargetMachine can't emit a file of this type");
//       return;
//     }
// 
//     pass.run(*module_ptr);
//     dest.flush();
//   }

  std::unique_ptr<llvm::Module>&& run() {
    return current_module -> codegen(this);
  }
};
