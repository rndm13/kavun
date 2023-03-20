#pragma once

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Analysis/ModuleSummaryAnalysis.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <stack>

#include "AST.hpp"

class interpreter_exception : std::exception {
  std::string info;

public:
  interpreter_exception(const Token &tok, const std::string &in);
  const char *what();
};

struct VariableData {
  llvm::Type *type;
  llvm::Value *value; // ptr to stack. Use load, alloca and store
  // Possibly make those fields std::unique_ptr?
  bool is_reference;
};

class ScopeData {
  std::unordered_map<std::string, VariableData> variables;

public:
  std::optional<llvm::BasicBlock*> to_continue;
  std::optional<llvm::BasicBlock*> to_break;
  bool is_for_loop = false;
  bool used_break = false;

  void add_variable(const Token &, llvm::Type *, llvm::Value *, bool);
  void assign_variable(const Token &, llvm::Value *);
  bool check_variable(const Token &) const;
  const VariableData &get_variable(const Token &) const;
};

class ScopeStack {
  std::list<ScopeData> data;

public:
  void add_scope();
  void pop_scope();

  void print();
  std::optional<ScopeData*> get_top_loop();
  ScopeData* top();
  void add_variable(const Token &, llvm::Type *, llvm::Value *, bool);
  void assign_variable(const Token &, llvm::Value *);
  bool check_variable(const Token &) const;
  const VariableData &get_variable(const Token &) const;
};

class CodeGenerator {
  std::unique_ptr<llvm::LLVMContext> the_context;
  std::unique_ptr<llvm::IRBuilder<>> the_builder;

  // TODO: add global scope
  ScopeStack scope_stack;

  std::unordered_map<std::string, llvm::Type *> type_lookup;

  llvm::Function *get_function(Token);
  llvm::Type *get_type(Token);

  static void warn(Token tok, const std::string_view &in);

public:
  std::unique_ptr<llvm::Module> the_module;

  void operator()(const AST::Module &);

  void operator()(const AST::StatementPtr &);
  llvm::Value *operator()(const AST::ExpressionPtr &);
  void operator()(const AST::TopLevelPtr &);

  llvm::Function *operator()(const AST::FnProto &);
  llvm::BasicBlock *operator()(const AST::Scope &, std::string block_name,
                               llvm::Function *parent);

  llvm::BasicBlock *operator()(const AST::Scope &, llvm::BasicBlock*);

  // Top level
  void operator()(const AST::Extern &);
  void operator()(const AST::FnDecl &);

  // Statements
  void operator()(const AST::Conditional &);
  void operator()(const AST::ForLoop &);
  void operator()(const AST::StatExpr &);
  void operator()(const AST::Return &);
  void operator()(const AST::VarDecl &);
  void operator()(const AST::Break &);
  void operator()(const AST::Continue &);

  // Expression
  llvm::Value *operator()(const AST::BinOperator &);
  llvm::Value *operator()(const AST::UnOperator &);
  llvm::Value *operator()(const AST::Literal &);
  llvm::Value *operator()(const AST::Variable &);
  llvm::Value *operator()(const AST::Grouping &);
  llvm::Value *operator()(const AST::FnCall &);

  llvm::Value *binOpFloat(const AST::BinOperator &, llvm::Value *,
                          llvm::Value *);
  llvm::Value *binOpInteger(const AST::BinOperator &, llvm::Value *,
                            llvm::Value *);
  llvm::Value *binOpBoolean(const AST::BinOperator &, llvm::Value *,
                            llvm::Value *);

  CodeGenerator();

  void optimize_module();
};
