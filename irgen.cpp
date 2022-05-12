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

static Type *yuc2LLVMType(CType *ctype) {
  Type *type, *base;
  int size = ctype->size;
  cout << "yuc2LLVMType size: " << size 
      << " kind: " << ctype->kind << endl;
  switch (ctype->kind) {
    case CType::IntegerType:
    cout << "\tIntegerType ";
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
    case CType::PointerType:
      cout << "\tPointerType ";
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
    // std::cout << "createGlobalVar name:" << name << endl;
    string name = yucNode->name;
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
    CType *ctype = var->type;
    if (ctype->kind != CType::IntegerType) {
      continue;
    }
    createGlobalVar(var);
  }
}

static std::vector<Type *> yuc2ParamTypes(Ast *funcNode) {
  std::vector<Type *> types;
  std::cout << "yuc2ParamTypes " << funcNode->name << std::endl;
  for (Ast *param = funcNode->params; param; param = param->next) {
    Type *type = yuc2LLVMType(param->type);
    types.push_back(type);
  }
  return types;
}

static Type *yuc2RetType(CNode *body) {
  CNode *cur = body;
  while(cur && cur->next) {
    cur = cur->next;
  }
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
  cout << "createFunc params: " << params.size();
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
void buildFunction(Ast *funcNode) {
  Function *fooFunc = createFunc(funcNode);
  BasicBlock *entry = BasicBlock::Create(*TheContext, "entry", fooFunc);
  Builder->SetInsertPoint(entry);
  Builder->CreateRet(Builder->getInt32(0));
  verifyFunction(*fooFunc);
}

static void emit_function(Ast *ast) {
  for (Ast *fn = ast; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    buildFunction(fn);
  }
}

void yuc::ir_gen(Ast *ast, std::ofstream &out, const string &moduleName) {
  if (!ast) {
    std::cerr << "no ast" << std::endl;
    return;
  }
  InitializeModule(moduleName);
  emit_data(ast);
  emit_function(ast);
  TheModule->print(errs(), nullptr);
  out << "yuc end" << std::endl;
}

