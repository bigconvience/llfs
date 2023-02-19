#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;

static void InitializeModule() {
	TheContext = std::make_unique<llvm::LLVMContext>();
	TheModule = std::make_unique<llvm::Module>("first module", *TheContext);
}

int main() {	
	InitializeModule();
	TheModule->print(llvm::outs(), nullptr);
	return 0;
}