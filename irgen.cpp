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

GlobalVariable *createGlob(Type *type, std::string name) {
    TheModule->getOrInsertGlobal(name, type);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    return gVar;
}

static void emit_data(Ast *ast) {
  GlobalVariable *gVar = createGlob(Builder->getInt32Ty(), ast->name);
  gVar->setAlignment(MaybeAlign(ast->align));
  gVar->setInitializer(Builder->getInt32(21));
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

