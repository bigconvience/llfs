#include "chibicc.h"
#include <iostream>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#define DUMP_OBJ 0

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;

static void print_type(Type *type) {
  if (!type) {
    return;
  }
  std::cout << "Type kind: " << ctypeKindString(type->kind) 
            << " align: " << type->align
            << std::endl;
}

static void print_obj(Obj *obj) {
  if (!obj) {
    return;
  }
  std::cout << "Obj name: " << obj->name 
            << " align: " << obj->align
            << std::endl;
  print_type(obj->ty);
}

static void dump_obj(Obj *obj) {
  if (!DUMP_OBJ) {
    return;
  }
  for (Obj *cur = obj; cur; cur = cur->next) {
    print_obj(cur);
  }
}

static void InitializeModule(const std::string &filename) {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>(filename, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

}

static llvm::Type *create_type(Type *ty) {
  llvm::Type *type;
  switch(ty->kind) {
  case TY_CHAR:
    type = Builder->getInt8Ty();
    break;
  case TY_SHORT:
    type = Builder->getInt16Ty();
    break;
  case TY_INT:
    type = Builder->getInt32Ty();
    break;
  case TY_LONG:
    type = Builder->getInt64Ty();
    break;
  case TY_FLOAT:
    type = Builder->getFloatTy();
    break;
  case TY_DOUBLE:
    type = Builder->getDoubleTy();
    break;
  default:
    type = Builder->getInt32Ty();
    break;
  }
  return type;
}

static llvm::GlobalValue::LinkageTypes create_linkage_type(Obj *var) {
  llvm::GlobalValue::LinkageTypes ret = llvm::GlobalValue::LinkageTypes::ExternalLinkage;
  if (var->is_static) {
    ret = llvm::GlobalValue::LinkageTypes::InternalLinkage;
  }
  return ret;
}

static void emit_global_var(Obj *var) {
  Type *ty = var->ty;
  if (ty->kind == TY_FUNC) {
    return;
  }
  std::string var_name = var->name;
  llvm::Type *type = create_type(ty);
  TheModule->getOrInsertGlobal(var_name, type);
  llvm::GlobalVariable *gvar = TheModule->getNamedGlobal(var_name);
  gvar->setAlignment(llvm::MaybeAlign(var->align));
  gvar->setDSOLocal(!var->is_static);
  gvar->setLinkage(create_linkage_type(var));
}

static void emit_data(Obj *prog) {
  if (!prog) {
    return;
  }
  emit_data(prog->next);
  emit_global_var(prog);
}

void gen_ir(Obj *prog, const std::string &filename) {
  InitializeModule(filename);
  dump_obj(prog);
  emit_data(prog);
  TheModule->print(llvm::outs(), nullptr);
}