#include "chibicc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Constants.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Instruction.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>


#define DUMP_OBJ 0

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;

static bool isAnnonVar(std::string &name) {
  int index = name.find(".L..");
  return index == 0;
}

static void InitializeModule(const std::string &filename) {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>(filename, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}

static llvm::LLVMContext &getLLVMContext() {
  return TheModule->getContext();
}

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
  for (Obj *cur = obj; cur; cur = cur->next) {
    print_obj(cur);
  }
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

static llvm::Constant *build_float(llvm::Type *type, Type *ctype, char *buf, int offset, Relocation *rel) {
  if (!buf) {
    return llvm::Constant::getNullValue(type);
  }
  float f_val = *(float *)(buf + offset);
  return llvm::ConstantFP::get(getLLVMContext(), llvm::APFloat(f_val));
}

static llvm::Constant *build_double(llvm::Type *type, Type *ctype, char *buf, int offset, Relocation *rel) {
  if (!buf) {
    return llvm::Constant::getNullValue(type);
  }
  double df_val = *(double *)(buf + offset);
  return llvm::ConstantFP::get(getLLVMContext(), llvm::APFloat(df_val));
}


static uint64_t read_integer_from_buf(const char *buf, int size) {
  switch(size) {
  case 1:
    return *buf;
  case 2:
    return *(uint16_t *)buf;
  case 4:
    return *(uint32_t *)buf;
  case 8:
    return *(uint64_t *)buf;
  default:
    return *buf;
  }
}

static llvm::Constant *build_integer(llvm::Type *type, Type *ctype, char *buf, int offset, Relocation *rel) {
  if (!buf) {
    return llvm::Constant::getNullValue(type);
  }
  return llvm::ConstantInt::get(type, read_integer_from_buf(buf + offset, ctype->size));
}

static llvm::Constant *build_constant(llvm::Type *type, Type *ctype, char *buf, int offset, Relocation *rel) {
  llvm::Constant *constant = nullptr;
  int size = ctype->size;
  switch(ctype->kind) {
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
    constant = build_integer(type, ctype, buf, offset, rel);
    break;
  case TY_FLOAT:
    constant = build_float(type, ctype, buf, offset, rel);
    break;
  case TY_DOUBLE:
    constant = build_double(type, ctype, buf, offset, rel);
    break;
  default:
    constant = Builder->getInt32(-1024);
    break;
  }
  return constant;
}

static llvm::Constant *build_initializer(llvm::Type *type, Obj *var) {
  llvm::Constant *constant = build_constant(type, var->ty, var->init_data, 0, var->rel);
  return constant;  
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
  llvm::Constant *initializer = build_initializer(type, var);
  gvar->setInitializer(initializer);
}

static void emit_data(Obj *prog) {
  if (!prog) {
    return;
  }
  emit_data(prog->next);
  emit_global_var(prog);
}

static llvm::Type *create_return_type(Type *return_ty) {
  llvm::Type *retTy = create_type(return_ty);
  return retTy;
}

static llvm::FunctionType * create_prototype(Obj *funcNode) {
  std::vector<llvm::Type *> types;
  Type *funcType = funcNode->ty;
  for (Type *paramType = funcType->params; paramType; paramType = paramType->next) {
    llvm::Type *type = create_type(paramType);
    types.push_back(type);
  }
  llvm::Type *RetTy = create_return_type(funcType->return_ty);
  bool isVarArg = funcType->is_variadic;
  if (types.empty()) {
    isVarArg = false;
  }
  llvm::FunctionType *functionType = llvm::FunctionType::get(RetTy, types, isVarArg);
  return functionType;
}


static llvm::Function *declare_func(Obj *funcNode) {
  assert(funcNode->ty->kind == TY_FUNC);

  std::string funcName = funcNode->name;
  llvm::FunctionType *funcType = create_prototype(funcNode);
  llvm::GlobalValue::LinkageTypes linkageType = create_linkage_type(funcNode);
  llvm::Function *func = llvm::Function::Create(funcType, linkageType, funcName, TheModule.get());

  return func;
}

static void StartFunction(llvm::Function *Fn, Obj *funcNode) {
  const char *name = Fn->getName().data();
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(getLLVMContext(), "", Fn);
  Builder->SetInsertPoint(entry);

}

static void buildFunctionBody(llvm::Function *Func, Obj *funcNode) {

}

static void FinishFunction(llvm::Function *Func, Obj *funcNode) {

  Builder->CreateRet(Builder->getInt32(-1024));
}

static void define_func(Obj *funcNode) {
  llvm::Function *fooFunc = declare_func(funcNode);
  // SetLLVMFunctionAttributes(fooFunc);
  if (!funcNode->is_definition) {
    return;
  }
  
  StartFunction(fooFunc, funcNode);
  buildFunctionBody(fooFunc, funcNode);
  FinishFunction(fooFunc, funcNode);
  llvm::verifyFunction(*fooFunc);
}

static void emit_function(Obj *fn) {
  if (!fn) {
    return;
  }
  emit_function(fn->next);
  if(fn->ty->kind != TY_FUNC) {
    return;
  }
  if (fn->is_definition) {
    define_func(fn);
  } else {
    declare_func(fn);
  }
}


void gen_ir(Obj *prog, const std::string &filename) {
  InitializeModule(filename);
  if (DUMP_OBJ) {
    dump_obj(prog);
  }
  emit_data(prog);
  emit_function(prog);

  TheModule->print(llvm::outs(), nullptr);
}