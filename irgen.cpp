#include "yuc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;
using namespace yuc;

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;

static void InitializeModule() {
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("first modlue", *TheContext);
  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

/**
 * https://llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 */ 
GlobalVariable *createGlobalVar(Type *type, Ast *ast) {
    string name = ast->name;
    TheModule->getOrInsertGlobal(name, type);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    gVar->setAlignment(MaybeAlign(ast->align));
    if (ast->is_preemptable) {
      gVar->setDSOLocal(true);
    }
    gVar->setInitializer(Builder->getInt32(21));
    gVar->setLinkage(static_cast<GlobalValue::LinkageTypes>(ast->linkage_type));
    return gVar;
}

static void emit_data(Ast *ast) {
  for (Ast *cur = ast; cur; cur = cur->next) {
    if (ast->is_function) {
      continue;      
    }
    GlobalVariable *gVar = createGlobalVar(Builder->getInt32Ty(), ast);
  }
}

void yuc::ir_gen(Ast *ast, std::ofstream &out) {
  if (!ast) {
    std::cerr << "no ast" << std::endl;
    return;
  }
  InitializeModule();
  emit_data(ast);

  TheModule->print(errs(), nullptr);
  out << "yuc end" << std::endl;
}

