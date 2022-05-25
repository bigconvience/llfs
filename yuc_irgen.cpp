#include "yuc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalValue.h"

using namespace llvm;
using namespace yuc;

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
// https://llvm.org/doxygen/IRBuilder_8h_source.html
static std::unique_ptr<IRBuilder<>> Builder;
static int stmt_level = 0;
static int stmt_count = 0;
static std::ofstream output("./test2/ast_ir.out");

static string buildSeperator(int count, string target) {
  string result = "";
  for (int i = 1; i < count; i++) {
    result.append("\t");
  }
  result.append(target);
  return result;
}

static void gen_addr(CNode *node) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_addr")
      << " node kind:" << node->kind << endl;
  cur_level++;

  switch(node->kind) {
    case CNode::CNodeKind::ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
  }
  --stmt_level;
}

static void cast(CType *from, CType *to) {

}

static void gen_expr(CNode *node) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_expr start, kind:") 
  << node->kind <<  endl; 
  cur_level++;
  switch(node->kind) {
    case CNode::CNodeKind::ND_NULL_EXPR:
      output << buildSeperator(cur_level, "ND_NULL_EXPR:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_COMMA:
      output << buildSeperator(cur_level, "ND_COMMA:") << node->kind << endl;
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      break;
    case CNode::CNodeKind::ND_COND:
      output << buildSeperator(cur_level, "ND_COND:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_ASSIGN:
      output << buildSeperator(cur_level, "ND_ASSIGN:") << node->kind << endl;
      gen_addr(node->lhs);
      gen_expr(node->rhs);

      break;
    case CNode::CNodeKind::ND_NUM:
      output << buildSeperator(cur_level, "ND_NUM:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_MEMZERO:
      output << buildSeperator(cur_level, "ND_MEMZERO:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_CAST:
      output << buildSeperator(cur_level, "ND_CAST:") << node->kind << endl;
      gen_expr(node->lhs);
      cast(node->lhs->type, node->type);
      break;
    case CNode::CNodeKind::ND_ADD:
      output << buildSeperator(cur_level, "ND_ADD:") << node->kind << endl;
      break;

  }
  stmt_level--;
  cur_level--;
  output << buildSeperator(cur_level, "gen_expr end") << endl; 
}

static void gen_stmt(CNode *node) {
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  output << buildSeperator(level, "gen_stmt start ==> ") << cur_count << endl;
  switch(node->kind) {
    case CNode::CNodeKind::ND_BLOCK: // 32
      output << buildSeperator(level+1, "ND_BLOCK:") << node->kind << "\n"; 
      for (CNode *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      break;
    case CNode::CNodeKind::ND_EXPR_STMT:
      output << buildSeperator(level+1, "ND_EXPR_STMT:")<< node->kind << "\n"; 
      gen_expr(node->lhs);
      break;

    case CNode::CNodeKind::ND_RETURN:
      output << buildSeperator(level+1, "ND_RETURN:") << node->kind << "\n"; 
      break;
    default:
      output << buildSeperator(level+1, "gen_stmt unknow kind: ") << node->kind << endl;
      break;
  }
  output << buildSeperator(level, "gen_stmt end <<<=== ") <<  cur_count << endl;
  --stmt_level;
}

static Type *yuc2LLVMType(CType *ctype) {
  Type *type, *base;
  int size = ctype->size;
  int cur_level = stmt_level;
  cout << buildSeperator(cur_level, "yuc2LLVMType size: ")
      << size << " kind: " << ctype->kind << " ";
  switch (ctype->kind) {
    case CType::TY_INT:
    cout << "IntegerType ";
      switch(size) {
        case 4:
          type = Builder->getInt32Ty();
          break;
        case 1:
          type = Builder->getInt8Ty();
          break;
        default:
          type = Builder->getInt32Ty();
          break;
      }
      break;
    case CType::TY_PTR:
      cout << "PointerType ";
      base = yuc2LLVMType(ctype->base);
      type = PointerType::get(base, 0);
      break;
    default:
      type = Builder->getInt32Ty();
      break;
  }
  
  cout << endl;
  return type;
}

static GlobalValue::LinkageTypes yuc2LinkageType(Ast *yucNode) {
  GlobalValue::LinkageTypes ret = GlobalValue::LinkageTypes::ExternalLinkage;
  if (yucNode->is_static) {
    ret = GlobalValue::LinkageTypes::InternalLinkage;
  }

  return ret;
}


static void InitializeModule(const string &moduleName) {
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>(moduleName, *TheContext);
  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}


/**
 * https://llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 * https://www.llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 */ 
GlobalVariable *createGlobalVar(Ast *yucNode) {
    string name = yucNode->name;
    CType *ctype = yucNode->type;
    std::cout << "createGlobalVar kind:" << ctype->kind << endl;
    Type *type = yuc2LLVMType(yucNode->type);
    TheModule->getOrInsertGlobal(name, type);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    gVar->setAlignment(MaybeAlign(yucNode->align));
    if (!yucNode->is_static) {
      gVar->setDSOLocal(true);
    }
    CValue *cvalue = yucNode->initializer;
    gVar->setInitializer(Builder->getInt32(cvalue->val));
    gVar->setLinkage(yuc2LinkageType(yucNode));
    return gVar;
}

static void emit_data(Ast *ast) {
  for (Ast *var = ast; var; var = var->next) {
    if (var->is_function || !var->is_definition) {
      continue;      
    }
    std::cout << "emit_data name:" << var->name << endl;

    CType *ctype = var->type;
    if (ctype->kind != CType::TY_INT) {
      continue;
    }
    createGlobalVar(var);
  }
}

static std::vector<Type *> yuc2ParamTypes(Ast *funcNode) {
  int cur_level = ++stmt_level;
  std::vector<Type *> types;
  std::cout << buildSeperator(cur_level, "yuc2ParamTypes ")
     << funcNode->name << std::endl;
  for (Ast *param = funcNode->params; param; param = param->next) {
    Type *type = yuc2LLVMType(param->type);
    types.push_back(type);
  }
  --stmt_level;
  return types;
}

static Type *yuc2RetType(CNode *body) {
  CNode *cur = body;
  while(cur && cur->next) {
    cout << "yuc2RetType middle kind " << cur->kind << endl;
    cur = cur->next;
  }
  cout << "yuc2RetType last kind " << cur->kind << endl;
  Type *type = yuc2LLVMType(cur->type);
  return type;
}

static std::vector<std::string> yuc2FuncArgs(Ast *funcNode) {
  std::vector<std::string> FuncArgs;
  for (Ast *param = funcNode->params; param; param = param->next) {
    FuncArgs.push_back(param->name);
  }
  return FuncArgs;
}

Function *createFunc(Ast *funcNode, bool isVarArg = false) {
  std::vector<Type *> params = yuc2ParamTypes(funcNode);
  cout << "createFunc params size:" << params.size() << endl;
  Type *RetTy = yuc2RetType(funcNode->body);
  FunctionType *funcType = FunctionType::get(RetTy, params, isVarArg);
  std::string funcName = funcNode->name;
  GlobalValue::LinkageTypes linkageType = yuc2LinkageType(funcNode);
  Function *fooFunc = Function::Create(funcType, linkageType, funcName, TheModule.get());
  return fooFunc;
}

void setFuncArgs(Function *Func, std::vector<std::string> FuncArgs) {
  unsigned Idx = 0;
  Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, ++Idx) {
      AI->setName(FuncArgs[Idx]);
    }
}

static void buildFunctionBody(Function *Func) {
  Function::arg_iterator AI, AE;
  std::vector<std::pair<Argument *, Value *>> args;
  // create alloca arg
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI) {
    Value *argValue = Builder->CreateAlloca(AI->getType(), nullptr);
    args.push_back(make_pair(AI, argValue));
  }
  // create store arg
  for(auto first = args.begin(); first < args.end(); ++first) {
    Builder->CreateStore(first->first, first->second);
  }
}

void buildFunction(Ast *funcNode) {
  cout << "buildFunction " << funcNode->name << endl;
  Function *fooFunc = createFunc(funcNode);
  BasicBlock *entry = BasicBlock::Create(*TheContext, "", fooFunc);
  Builder->SetInsertPoint(entry);
  buildFunctionBody(fooFunc);
  Builder->CreateRet(Builder->getInt32(0));
  verifyFunction(*fooFunc);
}

static void emit_text(Ast *ast) {
  for (Ast *fn = ast; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    stmt_count = 0;
    output << "emit_text, fn name:" << fn->name << endl;
    gen_stmt(fn->body);
    buildFunction(fn);
    output << endl;
  }
}

void yuc::ir_gen(Ast *ast, std::ofstream &out, const string &moduleName) {
  cout << "ir_gen start\n";
  if (!ast) {
    std::cerr << "no ast" << std::endl;
    return;
  }
  InitializeModule(moduleName);
  emit_data(ast);
  emit_text(ast);

  // Builder->CreateGlobalStringPtr(StringRef("Hello, world!\n"));
  TheModule->print(errs(), nullptr);
  out << "yuc end" << std::endl;
}

