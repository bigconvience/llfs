#include "chibicc.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;

static void InitializeModule() {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("first module", *TheContext);
}

void gen_ir(Obj *prog, const std::string &filename) {
  InitializeModule();
  TheModule->print(llvm::outs(), nullptr);
}