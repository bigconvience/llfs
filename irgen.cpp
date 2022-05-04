#include "yuc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;
using namespace yuc;

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
// https://llvm.org/doxygen/IRBuilder_8h_source.html
static std::unique_ptr<IRBuilder<>> Builder;

static Type *yuc2LLVMType(Ast *yucNode) {
  CType *ctype = yucNode->type;
  Type *type;
  int size = ctype->size;
  cout << "yuc2LLVMType size: " << size 
      << " type: " << ctype->kind << endl;
  switch (ctype->kind) {
    case CType::IntegerType:
      switch(size) {
        case 4:
          type = Builder->getInt32Ty();
          break;
      }
    break;
  }
  return type;
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
    Type *type = yuc2LLVMType(yucNode);
    TheModule->getOrInsertGlobal(name, type);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    gVar->setAlignment(MaybeAlign(yucNode->align));
    if (yucNode->is_preemptable) {
      gVar->setDSOLocal(true);
    }
    CValue *cvalue = yucNode->initializer;
    gVar->setInitializer(Builder->getInt32(cvalue->val));
    gVar->setLinkage(static_cast<GlobalValue::LinkageTypes>(yucNode->linkage_type));
    return gVar;
}

static void emit_data(Ast *ast) {
  for (Ast *cur = ast; cur; cur = cur->next) {
    if (ast->is_function) {
      continue;      
    }
    cout << "emit_data name: " << cur->name << endl;
    CType *ctype = cur->type;
    if (ctype->kind != CType::IntegerType) {
      continue;
    }
    createGlobalVar(cur);
  }
}

Function *createFunc(Type *RetTy, ArrayRef<Type *> Params, std::string Name, bool isVarArg = false) {
  FunctionType *funcType = FunctionType::get(RetTy, Params, isVarArg);
  Function *fooFunc = Function::Create(funcType, Function::ExternalLinkage, Name, TheModule.get());
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
  Function *fooFunc = createFunc(Builder->getInt32Ty(), {Builder->getInt32Ty()}, "main");
  BasicBlock *entry = BasicBlock::Create(*TheContext, "entry", fooFunc);
  Builder->SetInsertPoint(entry);
  Builder->CreateRet(Builder->getInt32(0));
  verifyFunction(*fooFunc);
  
  std::vector<std::string> FuncArgs;
  FuncArgs.push_back("left");
  //setFuncArgs(fooFunc, FuncArgs);
}

static void emit_function(Ast *ast) {
  for (Ast *fn = ast; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    cout << "emit_text name: " << fn->name << endl;
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

