#pragma once

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

#include <stack>

#include "AST.hpp"

class interpreter_exception : std::exception {
  std::string info;
public:
  interpreter_exception(const Token& tok, const std::string& in);
  virtual const char* what();
};

struct VariableData {
  llvm::Type*  type;
  llvm::Value* value; // ptr to stack. Use load, alloca and store
  // Possibly make those fields std::unique_ptr?
};

class ScopeData {
  std::unordered_map<std::string, VariableData> variables;
public:
  bool terminated;

  void add_variable(const Token&, llvm::Type*, llvm::Value*);
  void assign_variable(const Token&, llvm::Value*);
  bool check_variable(const Token&) const;
  const VariableData& get_variable(const Token&) const;
};

class ScopeStack {
  std::list<ScopeData> data;

  public:

  void add_scope();
  void pop_scope();

  void add_variable(const Token&, llvm::Type*, llvm::Value*);
  void assign_variable(const Token&, llvm::Value*);
  bool check_variable(const Token&) const;
  const VariableData& get_variable(const Token&) const;
};

class CodeGenerator {
  std::unique_ptr<llvm::LLVMContext> the_context;
  std::unique_ptr<llvm::IRBuilder<>> the_builder;

  // TODO: add global scope
  ScopeStack scope_stack;

  std::unordered_map<std::string, llvm::Type*> type_lookup;
  
  llvm::Function* get_function(Token);
  llvm::Type* get_type(Token);

  static void warn(Token tok, const std::string_view& in);

public:
  std::unique_ptr<llvm::Module>      the_module;

  void operator()(const AST::Module&); 

  void operator()(const AST::StatementPtr&); 
  llvm::Value* operator()(const AST::ExpressionPtr&); 
  void operator()(const AST::TopLevelPtr&); 

  llvm::Function*   operator()(const AST::FnProto&);
  llvm::BasicBlock* operator()(
      const AST::Scope&,
      std::string block_name,
      llvm::Function* parent);

  // Top level
  void operator()(const AST::Extern&);
  void operator()(const AST::FnDecl&);

  // Statements
  void operator()(const AST::Conditional&);
  void operator()(const AST::StatExpr&);
  void operator()(const AST::Return&);
  void operator()(const AST::VarDecl&);

  // Expression
  llvm::Value* operator()(const AST::BinOperator&);
  llvm::Value* operator()(const AST::UnOperator&);
  llvm::Value* operator()(const AST::Literal&);
  llvm::Value* operator()(const AST::Variable&);
  llvm::Value* operator()(const AST::Grouping&);
  llvm::Value* operator()(const AST::FnCall&);

  llvm::Value* binOpFloat
    (const AST::BinOperator&, llvm::Value*, llvm::Value*);
  llvm::Value* binOpInteger
    (const AST::BinOperator&, llvm::Value*, llvm::Value*);
  llvm::Value* binOpBoolean
    (const AST::BinOperator&, llvm::Value*, llvm::Value*);

  CodeGenerator();

  void optimize_module();

  // void print_mc(llvm::Module* module_ptr, std::string file_name = "output.o") {
  //   auto target_triple = llvm::sys::getDefaultTargetTriple();
  //   llvm::InitializeAllTargetInfos();
  //   llvm::InitializeAllTargets();
  //   llvm::InitializeAllTargetMCs();
  //   llvm::InitializeAllAsmParsers();
  //   llvm::InitializeAllAsmPrinters();

  //   std::string error = "failed to find requested target";
  //   auto target = llvm::TargetRegistry::lookupTarget(
  //       target_triple,
  //       error);

  //   if (!target) {
  //     throw std::runtime_error(error);
  //     return;
  //   }

  //   auto CPU = "generic";
  //   auto Features = "";

  //   llvm::TargetOptions opt;
  //   auto RM = std::optional<llvm::Reloc::Model>();

  //   auto TargetMachine = 
  //     llvm::Target->createTargetMachine(target_triple, CPU, Features, opt, RM);

  //   module_ptr -> setDataLayout(TargetMachine->createDataLayout());
  //   module_ptr -> setTargetTriple(target_triple);

  //   std::error_code EC;
  //   llvm::raw_fd_ostream dest(file_name, EC, llvm::sys::fs::OF_None);

  //   if (EC) {
  //     throw std::runtime_error(fmt::format("could not open file '{}'", file_name));
  //     return;
  //   }

  //   legacy::PassManager pass; // TODO: change this to smth else
  //   auto FileType = llvm::CGFT_ObjectFile;

  //   if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
  //     throw std::runtime_error("TargetMachine can't emit a file of this type");
  //     return;
  //   }

  //   pass.run(*module_ptr);
  //   dest.flush();
  // }
};
