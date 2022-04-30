#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "yuc.h"

using namespace llvm;

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

static void emit_data(yuc::Ast *ast) {
  GlobalVariable *gVar = createGlob(Builder->getInt32Ty(), ast->name);
  gVar->setInitializer(Builder->getInt32(ast->initializer));
}

void yuc::ir_gen(yuc::Ast *ast, std::ofstream &out) {
  InitializeModule();
  emit_data(ast);

  TheModule->print(errs(), nullptr);
  out << 'yuc end' << endl;
}

