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
 
void ScopeData::add_variable(const Token& tok, llvm::Type* type, llvm::Value* value, bool is_reference) {
  variables.insert(
      std::make_pair(
        tok.lexeme,
        VariableData{type, value, is_reference}));
}

bool ScopeData::check_variable(const Token& tok) const {
  return variables.contains(tok.lexeme);
}

const VariableData& ScopeData::get_variable(const Token& tok) const {
  return variables.at(tok.lexeme);
}

void ScopeStack::add_scope() {
  data.emplace_back();
}

void ScopeStack::pop_scope() {
  if (data.empty()) {
    throw std::runtime_error("cannot pop empty scope stack"); 
  } else {
    data.pop_back();
  }
}

bool ScopeStack::check_variable(const Token& name) const {
  for (auto& scope : data) {
    if (scope.check_variable(name))
      return true;
  }
  return false;
}

void ScopeStack::add_variable(
    const Token& name, 
    llvm::Type*  type,
    llvm::Value* value,
    bool is_reference) {
  if (data.empty()) {
    // TODO: probably make a global constant
    throw std::runtime_error("cannot add variable with empty stack"); 
  }
  data.back().add_variable(name, type, value, is_reference);
}

const VariableData& ScopeStack::get_variable(const Token& name) const {
  for (auto& scope : data) {
    if (scope.check_variable(name))
      return scope.get_variable(name);
  }
  throw interpreter_exception(name, "no such variable");
}

void ScopeData::assign_variable(const Token& name, llvm::Value* val) {
  if (!check_variable(name))
    throw interpreter_exception(name, "cannot assign value to unknown variable");
  auto var = get_variable(name);
  if (var.type != val -> getType())
    throw interpreter_exception(name, "cannot assign value to a variable with different type");
  var.value = val;

  variables.at(name.lexeme) = var;
}

void ScopeStack::assign_variable(const Token& name, llvm::Value* val) {
  for (auto& scope : data) {
    if (scope.check_variable(name)) {
      scope.assign_variable(name, val);
      return;
    }
  }
  throw interpreter_exception(name, "cannot assign value to unknown variable");
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
  if (!the_builder -> GetInsertBlock() -> getTerminator())
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
  llvm::Type* rt;
  if (proto.return_type) {
    rt = get_type(proto.return_type.value());
  } else {
    rt = llvm::Type::getVoidTy(*the_context);
  }

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
    if (!decl.proto.parameters[i].id)
      continue;
    scope_stack.add_variable(
        decl.proto.parameters[i].id.value(), 
        func -> getFunctionType() -> getParamType(i),
        func -> getArg(i),
        false);
  }

  [[maybe_unused]] auto scope = operator()(decl.body, "function_entry", func);

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

  if (if_block -> getTerminator() && else_block && else_block -> getTerminator()) {
    return;
  }

  if (!if_block -> getTerminator()) {
    the_builder -> SetInsertPoint(if_block);

    the_builder -> CreateBr(after_block);
  }

  if (else_block && !else_block -> getTerminator()) {
    the_builder -> SetInsertPoint(else_block);

    the_builder -> CreateBr(after_block);
  }

  the_builder -> SetInsertPoint(after_block);
}

void CodeGenerator::operator()(const AST::ForLoop& fl) {
  if (fl.variable)
    operator()(fl.variable.value());

  auto orig_block = the_builder -> GetInsertBlock();
  auto orig_point = the_builder -> GetInsertPoint();
  
  auto parent_fn  = orig_block -> getParent();

  auto loop_block = operator()(fl.body, "loop_block", parent_fn);

  auto loop_end_block = the_builder -> GetInsertBlock();

  the_builder -> SetInsertPoint(orig_block, orig_point);

  if (fl.condition) {
    llvm::BasicBlock* after_block =
      llvm::BasicBlock::Create(
          *the_context,
          "after_block");

    after_block -> insertInto(parent_fn);

    the_builder -> CreateCondBr(
        operator()(fl.condition.value()), loop_block, after_block);

    the_builder -> SetInsertPoint(loop_end_block);

    if (fl.iteration)
      operator()(fl.iteration.value());

    the_builder -> CreateCondBr(
        operator()(fl.condition.value()), loop_block, after_block);

    the_builder -> SetInsertPoint(after_block);
    return;
  }
  the_builder -> CreateBr(loop_block);

  the_builder -> SetInsertPoint(loop_end_block);

  if (fl.iteration)
    operator()(fl.iteration.value());

  the_builder -> CreateBr(loop_block);
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
  if (scope_stack.check_variable(var_decl.id))
    throw interpreter_exception(var_decl.id, "redefinition of a variable");

  auto expr_eval = operator()( var_decl.expression);
  auto type = get_type(var_decl.type);
  auto alloca = the_builder -> CreateAlloca(type, nullptr, var_decl.id.lexeme);
  the_builder -> CreateStore(expr_eval, alloca);

  scope_stack.add_variable(
    var_decl.id, 
    type,
    alloca,
    true);
}

llvm::Value* CodeGenerator::binOpFloat(
    const AST::BinOperator& bin_op,
    llvm::Value* lhs_eval,
    llvm::Value* rhs_eval) {
  switch (bin_op.op.type) {
    break; case TOK_MODULO:
      return the_builder -> CreateFRem(lhs_eval, rhs_eval, "remtmp");
    break; case TOK_PLUS:
      return the_builder -> CreateFAdd(lhs_eval, rhs_eval, "addtmp");
    break; case TOK_MINUS:
      return the_builder -> CreateFSub(lhs_eval, rhs_eval, "subtmp");
    break; case TOK_STAR:
      return the_builder -> CreateFMul(lhs_eval, rhs_eval, "multmp");
    break; case TOK_SLASH:
      return the_builder -> CreateFDiv(lhs_eval, rhs_eval, "divtmp");
    break; case TOK_LESS:
      return the_builder -> CreateFCmpOLT(lhs_eval, rhs_eval, "lesstmp");
    break; case TOK_LESS_EQUAL:
      return the_builder -> CreateFCmpOLE(lhs_eval, rhs_eval, "lesseqtmp");
    break; case TOK_GREATER:
      return the_builder -> CreateFCmpOGT(lhs_eval, rhs_eval, "greattmp");
    break; case TOK_GREATER_EQUAL:
      return the_builder -> CreateFCmpOGE(lhs_eval, rhs_eval, "greateqtmp");
    break; case TOK_BANG_EQUAL:
      return the_builder -> CreateFCmpONE(lhs_eval, rhs_eval, "noteqtmp");
    break; case TOK_EQUAL_EQUAL:
      return the_builder -> CreateFCmpOEQ(lhs_eval, rhs_eval, "eqtmp");
    break; default: 
      throw interpreter_exception(
          bin_op.op,
          "unknown binary operator for floating types");
  }
}

llvm::Value* CodeGenerator::binOpInteger(
    const AST::BinOperator& bin_op,
    llvm::Value* lhs_eval,
    llvm::Value* rhs_eval) {

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
      throw interpreter_exception(
          bin_op.op,
          "unknown binary operator for integer types");
  }
}

llvm::Value* CodeGenerator::binOpBoolean(
    const AST::BinOperator& bin_op,
    llvm::Value* lhs_eval,
    llvm::Value* rhs_eval) {
  switch (bin_op.op.type) {
    break; case TOK_OR:
      return the_builder -> CreateOr(lhs_eval, rhs_eval, "ortmp");
    break; case TOK_AND:
      return the_builder -> CreateAnd(lhs_eval, rhs_eval, "andtmp");
    break; default: 
      throw interpreter_exception(
          bin_op.op,
          "unknown binary operator for bool types");
  }
}

// Expression
llvm::Value* CodeGenerator::operator()(const AST::BinOperator& bin_op) { 
  if (bin_op.op.type == TOK_EQUAL) {
    auto var = std::get<AST::Variable>(*bin_op.lhs); // is always variable
                                                     // asserted by parser
    llvm::Value* rhs_eval = operator()(bin_op.rhs);
    auto data = scope_stack.get_variable(var.id);
    if (data.is_reference) {
      the_builder -> CreateStore(rhs_eval, data.value);
    } else {
      scope_stack.assign_variable(var.id, rhs_eval);
    }
    return rhs_eval;
  }

  llvm::Value* lhs_eval = operator()(bin_op.lhs);
  llvm::Value* rhs_eval = operator()(bin_op.rhs);
  if (!lhs_eval || !rhs_eval) {
    throw interpreter_exception(bin_op.op, "void cannot be an operand");
    return nullptr;
  }

  if (lhs_eval -> getType() -> isFloatingPointTy() || rhs_eval -> getType() -> isFloatingPointTy()) {
    return binOpFloat(bin_op, lhs_eval, rhs_eval);
  }

  if (lhs_eval -> getType() -> isIntegerTy(1) || rhs_eval -> getType() -> isIntegerTy(1)) {
    return binOpBoolean(bin_op, lhs_eval, rhs_eval);
  }

  if (lhs_eval -> getType() -> isIntegerTy() || rhs_eval -> getType() -> isIntegerTy()) {
    return binOpInteger(bin_op, lhs_eval, rhs_eval);
  }

  throw interpreter_exception(bin_op.op, "no available operators for those types");
}

llvm::Value* CodeGenerator::operator()(const AST::UnOperator& un_op) {
  llvm::Value* rhs_eval = operator()(un_op.rhs);
  
  if (!rhs_eval) {
    throw interpreter_exception(un_op.op, "void cannot be an operand");
    return nullptr;
  }

  if (un_op.op.type == TOK_BANG && rhs_eval -> getType() -> isIntegerTy(1)) {
    return the_builder -> CreateNot(rhs_eval, "nottmp");
  }

  if (un_op.op.type == TOK_MINUS && 
      rhs_eval -> getType() -> isIntegerTy()) {
    return the_builder -> CreateNeg(rhs_eval, "negtmp");
  }

  if (un_op.op.type == TOK_MINUS && 
      rhs_eval -> getType() -> isFloatingPointTy()) {
    return the_builder -> CreateFNeg(rhs_eval, "negtmp");
  }
  throw interpreter_exception(
      un_op.op,
      "unknown unary operator for this type");
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
  if (std::holds_alternative<double>(literal.value.literal)) {
    return llvm::ConstantFP::get(*the_context, llvm::APFloat(std::get<double>(literal.value.literal)));
  }
  throw std::runtime_error("this type is not implemented yet");
}

llvm::Value* CodeGenerator::operator()(const AST::Variable& var) {
  auto data = scope_stack.get_variable(var.id);
  if (data.is_reference) {
    return the_builder -> CreateLoad(data.type, data.value);
  }
  return data.value;
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
  type_lookup["double"] =
    llvm::Type::getDoubleTy(*the_context);

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
