#include "AST.hpp"

namespace AST {

FnProto::FnProto(const Token& _id, std::vector<VarDecl>&& _params, const Token& _return_type) 
  : id (_id), return_type(_return_type) {
  for (auto& param : _params) {
    parameters.emplace_back(std::move(param));
  }
}

Scope::Scope(std::vector<StatementPtr>&& input) : statements(input.size()) { 
  for (size_t ind = 0;ind < input.size(); ++ind) {
    statements[ind].reset(input[ind].release());
  }
} 

StatExpr::StatExpr(ExpressionPtr&& _expr) 
  : expr(std::forward<ExpressionPtr>(_expr)) { }


StatementPtr StatExpr::make(ExpressionPtr&& _expr) {
  return std::make_unique<Statement>(
      StatExpr(std::forward<ExpressionPtr>(_expr)));
}

Return::Return(std::optional<ExpressionPtr>&& _expression) 
  : opt_expression(
      std::forward<std::optional<ExpressionPtr>>(_expression)) { }

StatementPtr Return::make(std::optional<ExpressionPtr>&& _expression) {
  return std::make_unique<Statement>(
      Return(std::forward<std::optional<ExpressionPtr>>(_expression)));
}

VarDecl::VarDecl(const Token& _type, const Token& _id, ExpressionPtr&& _expr) 
  : type(_type), 
    id(_id), 
    opt_expression(std::forward<ExpressionPtr>(_expr)) { }

StatementPtr VarDecl::make(const Token& _type, const Token& _id, ExpressionPtr&& _expr) {
  return std::make_unique<Statement>(
      VarDecl(
        _type,
        _id,
        std::forward<ExpressionPtr>(_expr)));
}

Literal::Literal(const Token& t) : value(t) { }
ExpressionPtr Literal::make(const Token& t) {
  return std::make_unique<Expression>(
        Literal(t));
}

Variable::Variable(const Token& t) : id(t) { }
ExpressionPtr Variable::make(const Token& t) {
  return std::make_unique<Expression>(
        Variable(t));
}

BinOperator::BinOperator(
    ExpressionPtr&& _lhs,
    const Token& t,
    ExpressionPtr&& _rhs) 
  : op(t), 
    lhs(std::forward<ExpressionPtr>(_lhs)),
    rhs(std::forward<ExpressionPtr>(_rhs)) { }

ExpressionPtr BinOperator::make(ExpressionPtr&& _lhs, const Token& t, ExpressionPtr&& _rhs) {
  return std::make_unique<Expression>(
        BinOperator(
          std::forward<ExpressionPtr>(_lhs),
          t,
          std::forward<ExpressionPtr>(_rhs)));
}

UnOperator::UnOperator(const Token& t, ExpressionPtr&& _rhs) 
  : op(t), rhs(std::forward<ExpressionPtr>(_rhs)) { }

// std::size_t BinOperator::get_precedence() {
//   switch (op.type) {
//     case TOK_STAR: return 5;
//     case TOK_MODULO: return 5;
//     case TOK_SLASH: return 5;
//     case TOK_PLUS: return 6;
//     case TOK_MINUS: return 6;
//     case TOK_LESS_EQUAL: return 9;
//     case TOK_LESS: return 9;
//     case TOK_GREATER: return 9;
//     case TOK_GREATER_EQUAL: return 9;
//     case TOK_BANG_EQUAL: return 10;
//     case TOK_EQUAL_EQUAL: return 10;
//     case TOK_AND: return 14;
//     case TOK_OR: return 15;
//     default: return 999;
//   }
// }

ExpressionPtr UnOperator::make(const Token& t, ExpressionPtr&& _rhs) {
  return std::make_unique<Expression>(
        UnOperator(
          t,
          std::forward<ExpressionPtr>(_rhs)));
}

Grouping::Grouping(ExpressionPtr && _expr)
: expr(std::forward<ExpressionPtr>(_expr)) { }

ExpressionPtr Grouping::make(ExpressionPtr&& _expr) {
  return std::make_unique<Expression>(
        Grouping(std::forward<ExpressionPtr>(_expr)));
}

FnCall::FnCall(const Token& _id, std::vector<ExpressionPtr>&& input) 
  : id(_id), args(input.size()) { 
  for (size_t ind = 0; ind < input.size(); ++ind) {
    args[ind].reset(input[ind].release());
  }
}

ExpressionPtr FnCall::make(const Token& _id, std::vector<ExpressionPtr>&& _args) {
  return std::make_unique<Expression>(
        FnCall(
          _id,
          std::forward<std::vector<ExpressionPtr>>(_args)));
}

Extern::Extern(FnProto&& _proto)
  : proto(std::forward<FnProto>(_proto)) {}

TopLevelPtr Extern::make(FnProto&& _proto) {
  return std::make_unique<TopLevel>(
        Extern(std::forward<FnProto>(_proto)));
}

FnDecl::FnDecl(FnProto&& _proto, Scope&& _body)
  : proto(std::forward<FnProto>(_proto)), 
    body(std::forward<Scope>(_body)) { }

TopLevelPtr FnDecl::make(FnProto&& _proto, Scope&& _body) {
  return std::make_unique<TopLevel>(
    FnDecl(
      std::forward<FnProto>(_proto),
      std::forward<Scope>(_body)));
}

Conditional::Conditional(
    const Token& _id,
    ExpressionPtr&& _cond,
    Scope&& _if,
    std::optional<Scope>&& _else)
  : id(_id),
    condition(std::forward<ExpressionPtr>(_cond)),
    if_body(std::forward<Scope>(_if)),
    else_body(std::forward<std::optional<Scope>>(_else))
{ }

StatementPtr Conditional::make(
    const Token& _id,
    ExpressionPtr&& _cond,
    Scope&& _if,
    std::optional<Scope>&& _else) {
  return std::make_unique<Statement>(
    Conditional(
      _id,
      std::forward<ExpressionPtr>(_cond),
      std::forward<Scope>(_if),
      std::forward<std::optional<Scope>>(_else)));
}

Module::Module(const Token& _name, std::vector<TopLevelPtr>&& funcs) 
  : name(_name), functions(funcs.size()) {
  for (size_t ind = 0; ind < funcs.size(); ++ind) {
    functions.at(ind).reset(funcs.at(ind).release());
  }
}

} // AST namespace


// llvm::BasicBlock* ScopeAST::codegen(Interpreter* interp, llvm::Function* parent, std::string block_name = "scope_entry") {
//   llvm::BasicBlock *block = 
//     llvm::BasicBlock::Create(
//         *interp -> the_context,
//         block_name);
// 
//   block -> insertInto(parent);
// 
//   interp -> the_builder -> SetInsertPoint(block);
// 
//   interp -> add_scope(this);
// 
//   for (auto& stat : statements) {
//     stat -> codegen(interp);
//   }
// 
//   interp -> pop_scope();
// 
//   return block;
// }
// 
// void VariableDeclarationAST::codegen(Interpreter* interp) {
//   if (opt_expression)
//     interp -> set_named_value(id, opt_expression -> codegen(interp));
//   else interp -> set_named_value(id, nullptr);
// }
// 
// void SExpressionAST::codegen(Interpreter* interp) {
//   expr -> codegen(interp);
// }
// 
// llvm::Value* LiteralAST::codegen(Interpreter* interp) {
//   if (std::holds_alternative<std::uint32_t>(value.literal)) {
//     return interp -> the_builder -> 
//       getInt32(std::get<std::uint32_t>(value.literal)); // TODO: move this to int64 
//   }
//   if (std::holds_alternative<std::string>(value.literal)) {
//     return interp -> the_builder ->
//       CreateGlobalStringPtr(
//           std::get<std::string>(value.literal),
//           "strtmp",
//           0,
//           interp -> get_module());
//   }
//   if (std::holds_alternative<bool>(value.literal)) {
//     return interp -> the_builder ->
//       getInt1(std::get<bool>(value.literal));
//   }
//   throw interpreter_exception(value, "not implemented yet");
//   return nullptr;
// };
// 
// llvm::Value* VariableAST::codegen(Interpreter* interp) {
//   return interp -> get_named_value(id);
// }
// 
// llvm::Value* BinaryOperationAST::codegen(Interpreter* interp) {
//   llvm::Value* lhs_eval = lhs -> codegen(interp);
//   llvm::Value* rhs_eval = rhs -> codegen(interp);
//   if (!lhs_eval || !rhs_eval) {
//     throw interpreter_exception(op, "void cannot be an operand");
//     return nullptr;
//   }
// 
//   auto* builder = interp -> the_builder.get();
//   
//   switch (op.type) {
//     break; case TOK_MODULO:
//       return builder -> CreateSRem(lhs_eval, rhs_eval, "remtmp");
//     break; case TOK_PLUS:
//       return builder -> CreateAdd(lhs_eval, rhs_eval, "addtmp");
//     break; case TOK_MINUS:
//       return builder -> CreateSub(lhs_eval, rhs_eval, "subtmp");
//     break; case TOK_STAR:
//       return builder -> CreateMul(lhs_eval, rhs_eval, "multmp");
//     break; case TOK_SLASH:
//       return builder -> CreateSDiv(lhs_eval, rhs_eval, "divtmp");
//     break; case TOK_OR:
//       return builder -> CreateOr(lhs_eval, rhs_eval, "ortmp");
//     break; case TOK_AND:
//       return builder -> CreateAnd(lhs_eval, rhs_eval, "andtmp");
//     break; case TOK_LESS:
//       return builder -> CreateICmpSLT(lhs_eval, rhs_eval, "lesstmp");
//     break; case TOK_LESS_EQUAL:
//       return builder -> CreateICmpSLE(lhs_eval, rhs_eval, "lesseqtmp");
//     break; case TOK_GREATER:
//       return builder -> CreateICmpSGT(lhs_eval, rhs_eval, "greattmp");
//     break; case TOK_GREATER_EQUAL:
//       return builder -> CreateICmpSGE(lhs_eval, rhs_eval, "greateqtmp");
//     break; case TOK_BANG_EQUAL:
//       return builder -> CreateICmpNE(lhs_eval, rhs_eval, "noteqtmp");
//     break; case TOK_EQUAL_EQUAL:
//       return builder -> CreateICmpEQ(lhs_eval, rhs_eval, "eqtmp");
//     break; default: 
//       throw std::runtime_error(fmt::format("Unknown binary operator '{}'", op.lexeme));
//       return nullptr;
//   }
// }
// 
// llvm::Value* UnaryOperationAST::codegen(Interpreter* interp) {
//   llvm::Value* rhs_eval = rhs -> codegen(interp);
//   
//   if (!rhs_eval) {
//     throw interpreter_exception(op, "void cannot be an operand");
//     return nullptr;
//   }
// 
//   // TODO: add for different types
//   // e.g. CreateFNeg for float
//   switch (op.type) {
//     break; case TOK_MINUS:
//       return interp -> the_builder -> 
//         CreateNeg(rhs_eval, "negtmp");
//     break; case TOK_BANG:
//       return interp -> the_builder -> 
//         CreateNot(rhs_eval, "nottmp");
//     break; default: 
//       throw std::runtime_error(fmt::format("Unknown unary operator '{}'", op.lexeme));
//       return nullptr;
//   }
// }
// 
// llvm::Value* GroupingAST::codegen(Interpreter* interp) {
//   return expr -> codegen(interp);
// }
// 
// llvm::Value* FunctionCallAST::codegen(Interpreter* interp) {
// 
//   llvm::Function* func = interp -> get_function(id);
//   if (!func) {
//     return nullptr;
//   }
// 
//   
//   std::vector<llvm::Value*> arg_vals;
// 
//   arg_vals.reserve(args.size());
//   for (auto& arg : args) {
//     auto arg_eval = arg -> codegen(interp);
// 
//     if (!arg_eval) {
//       throw interpreter_exception(id, "void cannot be an argument");
//       return nullptr;
//     }
// 
//     arg_vals.push_back(arg_eval);
//   }
// 
//   if (func -> getReturnType() -> isVoidTy()) {
//     interp -> the_builder -> CreateCall(func, arg_vals);
//     return nullptr;
//   }
//   return interp -> the_builder -> CreateCall(func, arg_vals, "calltmp");
// }
// 
// void ReturnAST::codegen(Interpreter* interp) {
//   if (opt_expression) {
//     interp -> the_builder -> CreateRet(opt_expression -> codegen(interp));
//   } else {
//     interp -> the_builder -> CreateRetVoid();
//   }
// }
// 
// llvm::Function* FunctionPrototypeAST::codegen(Interpreter* interp) {
//   // setting parameter types
//   std::vector<llvm::Type*> parameter_types{};
//   parameter_types.reserve(parameters.size());
//   for (auto& param : parameters) 
//     parameter_types.push_back(interp -> get_type(param -> type));
// 
//   // return type
//   llvm::Type* rt = interp -> get_type(return_type);
// 
//   // function signature
//   llvm::FunctionType* func_type = 
//     llvm::FunctionType::get(
//         rt,
//         parameter_types,
//         false);
//   
//   // function
//   llvm::Function* func = 
//     llvm::Function::Create(
//         func_type,
//         llvm::Function::ExternalLinkage,
//         id.lexeme,
//         interp -> get_module());
// 
//   return func;
// }
// 
// llvm::Function* ExternFunctionAST::codegen(Interpreter* interp) {
//   auto func = proto -> codegen(interp);
//   return func;
// }
// 
// llvm::Function* FunctionDeclarationAST::codegen(Interpreter* interp) {
//   auto func = proto -> codegen(interp);
// 
//   for (size_t i = 0; i < proto -> parameters.size(); ++i){
//     body -> named_values[proto -> parameters[i] -> id.lexeme] = func -> getArg(i);
//   }
// 
//   auto scope = body -> codegen(interp, func, "function_entry");
// 
//   return func;
// }
// 
// void ConditionalAST::codegen(Interpreter* interp) {
//   auto cond_eval = condition -> codegen(interp);
//   if (cond_eval == nullptr) {
//     throw interpreter_exception(id, "condition cannot be void");
//     return;
//   }
// 
//   auto orig_block = interp -> the_builder -> GetInsertBlock();
//   auto orig_point = interp -> the_builder -> GetInsertPoint();
// 
//   auto parent_fn  = orig_block -> getParent();
// 
//   auto if_block = if_body -> codegen(interp, parent_fn, "then_block");
// 
//   interp -> the_builder -> SetInsertPoint(orig_block, orig_point);
// 
//   llvm::BasicBlock* else_block = nullptr;
//   if (else_body) {
//     else_block = else_body -> codegen(interp, parent_fn, "else_block");
//   }
// 
//   interp -> the_builder -> SetInsertPoint(orig_block);
// 
//   llvm::BasicBlock* after_block =
//     llvm::BasicBlock::Create(
//         *interp -> the_context,
//         "after_block");
// 
//   after_block -> insertInto(parent_fn);
//   
//   interp -> the_builder -> SetInsertPoint(orig_block, orig_point);
// 
//   if (else_body)
//     interp -> the_builder -> CreateCondBr(cond_eval, if_block, else_block);
//   else
//     interp -> the_builder -> CreateCondBr(cond_eval, if_block, after_block);
// 
//   interp -> the_builder -> SetInsertPoint(if_block);
// 
//   interp -> the_builder -> CreateBr(after_block);
// 
//   if (else_body) {
//     interp -> the_builder -> SetInsertPoint(else_block);
// 
//     interp -> the_builder -> CreateBr(after_block);
//   }
// 
//   interp -> the_builder -> SetInsertPoint(after_block);
// }
// 
// std::unique_ptr<llvm::Module>&& ModuleAST::codegen(Interpreter* interp) {
//   the_module = std::make_unique<llvm::Module>(name.lexeme, *interp -> the_context);
//   for (auto& func : functions) {
//     func -> codegen(interp);
//   }
//   // interp -> optimize_module(the_module.get());
//   return std::move(the_module);
// }
// 
// void Interpreter::optimize_module(llvm::Module* module_ptr) {
//   // Create the analysis managers.
//   llvm::LoopAnalysisManager LAM;
//   llvm::FunctionAnalysisManager FAM;
//   llvm::CGSCCAnalysisManager CGAM;
//   llvm::ModuleAnalysisManager MAM;
// 
//   // Create the new pass manager builder.
//   // Take a look at the PassBuilder constructor parameters for more
//   // customization, e.g. specifying a TargetMachine or various debugging
//   // options.
//   llvm::PassBuilder PB;
// 
//   // Register all the basic analyses with the managers.
//   PB.registerModuleAnalyses(MAM);
//   PB.registerCGSCCAnalyses(CGAM);
//   PB.registerFunctionAnalyses(FAM);
//   PB.registerLoopAnalyses(LAM);
//   PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
// 
//   // Create the pass manager.
//   // This one corresponds to a typical -O2 optimization pipeline.
//   llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(
//       llvm::PassBuilder::OptimizationLevel::O2);
// 
//   // Optimize the IR!
//   MPM.run(*module_ptr, MAM); 
// }
