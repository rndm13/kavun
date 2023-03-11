#include "code_generator.hpp"

interpreter_exception::interpreter_exception(const Token& tok, const std::string& in)
  : info(fmt::format("('{}', line {}, col {}) : {}",
        tok.lexeme, tok.line, tok.col, in)) { }

void CodeGenerator::warn(Token tok, const std::string_view& in) {
  fmt::print("[WARNING]  ('{}', line {}, col {}) : {}",
        tok.lexeme, tok.line, tok.col, in);
}

const char* interpreter_exception::what() {
  return info.c_str();
}

void ScopeStack::add_scope() {
  emplace();
}

void ScopeStack::pop_scope() {
  if (empty()) {
    throw std::runtime_error("cannot pop empty scope stack"); 
  } else {
    pop();
  }
}

void ScopeStack::set_named_value(
    const Token& name, 
    llvm::Value* value) {
  if (empty()) {
    // TODO: probably make a global constant
    throw std::runtime_error("cannot add named value with empty stack"); 
  }
  top().insert(
      std::make_pair(
        name.lexeme,
        value));
}

llvm::Value* ScopeStack::get_named_value(const Token& name) {
  for (auto& m : c) {
    if (m.contains(name.lexeme))
      return m[name.lexeme];
  }
  throw interpreter_exception(name, "no such named value");
}

llvm::Function* CodeGenerator::get_function(Token identifier) {
  auto result = the_module -> getFunction(identifier.lexeme);
  if (result)
    return result;
  throw interpreter_exception(identifier, "function not found");
  return nullptr;
}

llvm::Type* CodeGenerator::get_type(Token identifier) {
  try {
    return type_lookup.at(identifier.lexeme);
  } catch (std::out_of_range&) {
    throw interpreter_exception(identifier, "unknown type");
  }
}

void CodeGenerator::operator()(const AST::StatementPtr& statement) {
  std::visit(*this, *statement);
}

llvm::Value* CodeGenerator::operator()(const AST::ExpressionPtr& expression) {
  return std::visit(*this, *expression);
}

void CodeGenerator::operator()(const AST::TopLevelPtr& top_level) {
  std::visit(*this, *top_level);
}

llvm::Function* CodeGenerator::operator()(
    const AST::FnProto& proto) {
  // setting parameter types
  std::vector<llvm::Type*> parameter_types{};
  parameter_types.reserve(proto.parameters.size());
  for (auto& param : proto.parameters) 
    parameter_types.push_back(get_type(param.type));

  // return type
  llvm::Type* rt = get_type(proto.return_type);

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
        proto.id.lexeme,
        the_module.get());

  return func;
}

llvm::BasicBlock* CodeGenerator::operator()(
    const AST::Scope& scope,
    std::string block_name,
    llvm::Function* parent) {

  llvm::BasicBlock* block = 
    llvm::BasicBlock::Create(
        *the_context,
        block_name);
  block -> insertInto(parent);

  the_builder -> SetInsertPoint(block);

  scope_stack.add_scope();

  for (const auto& stat : scope.statements) {
    operator()(stat);
  }

  scope_stack.pop_scope();

  return block;
}

// Top level
void CodeGenerator::operator()(const AST::Extern& ext) { 
  operator()(ext.proto);
}
void CodeGenerator::operator()(const AST::FnDecl& decl) { 
  auto func = operator()(decl.proto);

  scope_stack.add_scope(); // TODO: find a better solution?
                           // to not being able to set argument names to scope

  for (size_t i = 0; i < decl.proto.parameters.size(); ++i) {
    scope_stack.set_named_value(
        decl.proto.parameters[i].id, 
        std::move(func -> getArg(i)));
  }

  auto scope = operator()(decl.body, "function_entry", func);

  scope_stack.pop_scope(); // read above todo
  if (!the_builder -> GetInsertBlock() -> getTerminator()) {
    throw interpreter_exception(decl.proto.id, "function is not terminated (not all codepaths return a value)");
  } 
}

// Statements
void CodeGenerator::operator()(const AST::Conditional& cond) {
  auto cond_eval = operator()(cond.condition);

  if (cond_eval == nullptr) {
    throw interpreter_exception(cond.id, "condition cannot be void");
    return;
  }

  auto orig_block = the_builder -> GetInsertBlock();
  auto orig_point = the_builder -> GetInsertPoint();

  auto parent_fn  = orig_block -> getParent();

  auto if_block = operator()(cond.if_body, "then_block", parent_fn);

  the_builder -> SetInsertPoint(orig_block, orig_point);

  llvm::BasicBlock* else_block = nullptr;
  if (cond.else_body) {
    else_block = operator()(
        cond.else_body.value(), 
        "else_block", 
        parent_fn);
  }

  the_builder -> SetInsertPoint(orig_block);

  llvm::BasicBlock* after_block =
    llvm::BasicBlock::Create(
        *the_context,
        "after_block");

  after_block -> insertInto(parent_fn);
  
  the_builder -> SetInsertPoint(orig_block, orig_point);

  if (else_block) {
    the_builder -> CreateCondBr(cond_eval, if_block, else_block);
  } else {
    the_builder -> CreateCondBr(cond_eval, if_block, after_block);
  }

  if (!if_block -> getTerminator()) {
    the_builder -> SetInsertPoint(if_block);

    the_builder -> CreateBr(after_block);
  }

  if (else_block && else_block -> getTerminator()) {
    the_builder -> SetInsertPoint(else_block);

    the_builder -> CreateBr(after_block);
  }

  the_builder -> SetInsertPoint(after_block);
}

void CodeGenerator::operator()(const AST::StatExpr& stat_expr) {
  operator()(stat_expr.expr);
  // TODO: Warn if unused expression
}

void CodeGenerator::operator()(const AST::Return& ret) {
  if (ret.opt_expression) {
    the_builder -> CreateRet(operator()(
          ret.opt_expression.value()));
  } else {
    the_builder -> CreateRetVoid();
  }
}

void CodeGenerator::operator()(const AST::VarDecl& var_decl) {
  if (!var_decl.opt_expression) 
    scope_stack.set_named_value(
      var_decl.id, nullptr);
  else
    scope_stack.set_named_value(
      var_decl.id, operator()(var_decl.opt_expression.value()));
}

// Expression
llvm::Value* CodeGenerator::operator()(const AST::BinOperator& bin_op) { 
  llvm::Value* lhs_eval = operator()(bin_op.lhs);
  llvm::Value* rhs_eval = operator()(bin_op.rhs);
  if (!lhs_eval || !rhs_eval) {
    throw interpreter_exception(bin_op.op, "void cannot be an operand");
    return nullptr;
  }

  switch (bin_op.op.type) {
    break; case TOK_MODULO:
      return the_builder -> CreateSRem(lhs_eval, rhs_eval, "remtmp");
    break; case TOK_PLUS:
      return the_builder -> CreateAdd(lhs_eval, rhs_eval, "addtmp");
    break; case TOK_MINUS:
      return the_builder -> CreateSub(lhs_eval, rhs_eval, "subtmp");
    break; case TOK_STAR:
      return the_builder -> CreateMul(lhs_eval, rhs_eval, "multmp");
    break; case TOK_SLASH:
      return the_builder -> CreateSDiv(lhs_eval, rhs_eval, "divtmp");
    break; case TOK_OR:
      return the_builder -> CreateOr(lhs_eval, rhs_eval, "ortmp");
    break; case TOK_AND:
      return the_builder -> CreateAnd(lhs_eval, rhs_eval, "andtmp");
    break; case TOK_LESS:
      return the_builder -> CreateICmpSLT(lhs_eval, rhs_eval, "lesstmp");
    break; case TOK_LESS_EQUAL:
      return the_builder -> CreateICmpSLE(lhs_eval, rhs_eval, "lesseqtmp");
    break; case TOK_GREATER:
      return the_builder -> CreateICmpSGT(lhs_eval, rhs_eval, "greattmp");
    break; case TOK_GREATER_EQUAL:
      return the_builder -> CreateICmpSGE(lhs_eval, rhs_eval, "greateqtmp");
    break; case TOK_BANG_EQUAL:
      return the_builder -> CreateICmpNE(lhs_eval, rhs_eval, "noteqtmp");
    break; case TOK_EQUAL_EQUAL:
      return the_builder -> CreateICmpEQ(lhs_eval, rhs_eval, "eqtmp");
    break; default: 
      throw std::runtime_error(
          fmt::format("Unknown binary operator '{}'", bin_op.op.lexeme));
  }
}

llvm::Value* CodeGenerator::operator()(const AST::UnOperator& un_op) {
  llvm::Value* rhs_eval = operator()(un_op.rhs);
  
  if (!rhs_eval) {
    throw interpreter_exception(un_op.op, "void cannot be an operand");
    return nullptr;
  }

  // TODO: add for different types
  // e.g. CreateFNeg for float
  switch (un_op.op.type) {
    break; case TOK_MINUS:
      return the_builder -> 
        CreateNeg(rhs_eval, "negtmp");
    break; case TOK_BANG:
      return the_builder -> 
        CreateNot(rhs_eval, "nottmp");
    break; default: 
      throw std::runtime_error(
          fmt::format("Unknown unary operator '{}'", un_op.op.lexeme));
      return nullptr;
  }
}

llvm::Value* CodeGenerator::operator()(const AST::Literal& literal) {
  if (std::holds_alternative<std::uint32_t>(literal.value.literal)) {
    return the_builder -> getInt32(std::get<std::uint32_t>(
              literal.value.literal)); // TODO: move this to int64 
  }
  if (std::holds_alternative<std::string>(literal.value.literal)) {
    return
      the_builder ->
        CreateGlobalStringPtr(
          std::get<std::string>(literal.value.literal),
          "strtmp",
          0,
          the_module.get());
  }
  if (std::holds_alternative<bool>(literal.value.literal)) {
    return the_builder ->
        getInt1(std::get<bool>(literal.value.literal));
  }
  throw std::runtime_error("this type is not implemented yet");
}

llvm::Value* CodeGenerator::operator()(const AST::Variable& var) {
  return scope_stack.get_named_value(var.id);
}

llvm::Value* CodeGenerator::operator()(const AST::Grouping& group) {
  return operator()(group.expr);
}

llvm::Value* CodeGenerator::operator()(const AST::FnCall& fn) { 
  llvm::Function* func = get_function(fn.id);
  if (!func) {
    return nullptr;
  }
  
  std::vector<llvm::Value*> arg_vals;

  arg_vals.reserve(fn.args.size());
  for (const auto& arg : fn.args) {
    auto arg_eval = operator()(arg);

    if (!arg_eval) {
      throw interpreter_exception(fn.id, "void cannot be an argument");
      return nullptr;
    }

    arg_vals.push_back(arg_eval);
  }

  if (func -> getReturnType() -> isVoidTy()) {
    the_builder -> CreateCall(func, arg_vals);
    return nullptr;
  }

  return the_builder -> CreateCall(func, arg_vals, "calltmp");
}

void CodeGenerator::operator()(const AST::Module& mod) {
  the_module = std::make_unique<llvm::Module>(
      mod.name.lexeme, 
      *the_context);
  for (auto& func : mod.functions) {
    operator()(func);
  }
//   if (llvm::verifyModule(*the_module), &llvm::outs()) {
//     throw std::runtime_error("LLVM verification error");
//   }
  optimize_module();
}

CodeGenerator::CodeGenerator() {
  the_context = std::make_unique<llvm::LLVMContext>();
  the_builder = std::make_unique<llvm::IRBuilder<>>(*the_context);
  type_lookup["i32"]  = llvm::Type::getInt32Ty(*the_context);
  type_lookup["bool"] = llvm::Type::getInt1Ty(*the_context);
  type_lookup["void"] = llvm::Type::getVoidTy(*the_context);
  // TODO: change string to a class
  type_lookup["string"] = 
    llvm::PointerType::get(llvm::Type::getInt8Ty(*the_context), 0); 

  // TODO: add double
}

void CodeGenerator::optimize_module() {
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
  MPM.run(*the_module, MAM); 
}
