#include "chibicc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Constants.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
// https://llvm.org/doxygen/IRBuilder_8h_source.html
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, char *> annonInitData;
static std::map<std::string, llvm::GlobalVariable*> strLiteralCache;

static int stmt_level = 0;
static int stmt_count = 0;
static std::ofstream output("./test2/ast.out");
static llvm::Constant *buffer2Constants(llvm::Type *Ty, Type *ctype, Relocation *rel, char *buf, int offset);
static llvm::Constant *yuc2Constants(llvm::Type *Ty, Obj *gvarNode);
static llvm::GlobalVariable *createGlobalVar(Obj *yucNode);
static llvm::Type *yuc2LLVMType(Type *ctype);
static llvm::Type *getTypeWithArg(Type *ctype);
static llvm::Constant *getOffset(long offset);

static std::map<Obj *, llvm::Value*> scopeVars;
static std::map<Obj *, llvm::Constant*> globalVars;

static void genStore(llvm::Value *V, llvm::Value *Addr);

static llvm::Type *getTypeWithArg(Type *ty) {
  llvm::Type *localType;
  if (ty->kind == TY_BOOL) {
    localType = Builder->getInt8Ty();
  } else {
    localType = yuc2LLVMType(ty);
  }
  return localType;
}

static llvm::Value *getScopeVar(Obj *var) {
  return scopeVars[var];
}

static void putScopeVar(Obj *var, llvm::Value *V) {
  scopeVars[var] = V;
}

static llvm::Constant *getGlobalVar(Obj *var) {
  return globalVars[var];
}

static void putGlobalVar(Obj *var, llvm::Constant *V) {
  globalVars[var] = V;
}

static llvm::Value *findVar(Obj *var) {
  if (scopeVars[var]) {
    return scopeVars[var];
  }
  return globalVars[var];
}

static void clearScopeVars() {
  scopeVars.clear();
}

static llvm::Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  // auto FI = FunctionProtos.find(Name);
  // if (FI != FunctionProtos.end())
  //   return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

class IRGenModule {
private:
  llvm::Module &TheModule;
  llvm::LLVMContext &VMContext;

public:
  IRGenModule(llvm::Module &M);
  ~IRGenModule();
  
  llvm::GlobalVariable *GetAddrOfConstantCString(const std::string &Str, const char *GlobalName);

  llvm::LLVMContext &getLLVMContext() { return VMContext; }
  llvm::Module &getModule() { return TheModule; }

};
static IRGenModule &CGM();

static llvm::Value *gen_expr(Node *node);
static void gen_stmt(Node *node);

static llvm::Value *retValue = nullptr;
static llvm::Value *allocatedRetValue = nullptr;

static bool isAnnonVar(std::string &name) {
  int index = name.find(".L..");
  return index == 0;
}

static llvm::Type *getPaddingType(int PadSize) {
  llvm::Type *Ty = Builder->getInt8Ty();
  if (PadSize > 1) {
    Ty = llvm::ArrayType::get(Ty, PadSize);
  }
  return Ty;
}

static llvm::Constant *getPadding(int PadSize) {
  llvm::Type *Ty = getPaddingType(PadSize);
  return llvm::UndefValue::get(Ty);
}

static std::string buildSeperator(int count, std::string target) {
  std::string result = "";
  for (int i = 1; i < count; i++) {
    result.append("  ");
  }
  result.append(target);
  return result;
}

static llvm::Value* load(Type *ty, llvm::Value *originValue) {
  llvm::Type *type = yuc2LLVMType(ty);
  return Builder->CreateLoad(type, originValue);
}

static llvm::Value *gen_RValue(Node *node) {
  int cur_level = ++stmt_level;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(cur_level, "gen_RValue start:" + kindStr) << std::endl;
  Obj *var = node->var;
  std::string varName = var->name;
  llvm::Value *Addr = TheModule->getNamedGlobal(varName);
  --stmt_level;
  output << buildSeperator(cur_level, "gen_RValue end") << std::endl;
  return load(var->ty, Addr);  
}

static llvm::Value *gen_addr(Node *node) {
  int cur_level = ++stmt_level;
  llvm::Value *Addr = nullptr;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(cur_level, "gen_addr start:" + kindStr) << std::endl;
  switch(node->kind) {
    case NodeKind::ND_VAR:
    Obj *var = node->var;
    std::string typeStr = ctypeKindString(var->ty->kind);
    std::string varName = var->name;
    output << buildSeperator(cur_level + 1, "var type:" + typeStr)
      << " " << varName << std::endl;
    if (isAnnonVar(varName)) {
      const std::string &Str = annonInitData[varName];
      Addr = CGM().GetAddrOfConstantCString(Str, nullptr);
    } else if (var->is_local){
      llvm::Value *originValue = getScopeVar(var);
      if (!originValue) {
        output << " empty originValue " << std::endl;
      }
      llvm::Type *type = getTypeWithArg(var->ty);
      Addr = Builder->CreateLoad(type, originValue);
      if (var->ty->kind == TY_BOOL) {
        Addr = Builder->CreateTrunc(Addr, Builder->getInt1Ty());
      }
    } else {
      llvm::Type *type = yuc2LLVMType(var->ty);
      Addr = TheModule->getNamedGlobal(varName);
    }
    break;
  }
  --stmt_level;
  output << buildSeperator(cur_level, "gen_addr end") << std::endl;
  return Addr;
}

static llvm::Value* cast(llvm::Value *Base, Type *from, Type *to) {
  TypeKind fromKind = from->kind;
  TypeKind toKind = to->kind;

  std::string fromTypeStr = ctypeKindString(fromKind);
  std::string toTypeStr = ctypeKindString(toKind);
  // output << buildSeperator(stmt_level, "  cast: ");
  // output << "fromType: " << fromTypeStr << " toType: " << toTypeStr << std::endl;
  
  if (fromKind == toKind) {
    return Base;
  }

  llvm::Type *targetTy = yuc2LLVMType(to);
  llvm::Value *target = Base;
  if (fromKind == TY_ARRAY && toKind == TY_PTR) {
    llvm::SmallVector<llvm::Constant*, 8> IndexValues;
    llvm::Constant *ZERO = llvm::ConstantInt::get(Builder->getInt64Ty(), 0);
    IndexValues.push_back(ZERO);
    IndexValues.push_back(ZERO);

    if (auto *array = dyn_cast<llvm::Constant>(Base)) {
      llvm::Type *BaseValueTy = yuc2LLVMType(from);
      target = llvm::ConstantExpr::getInBoundsGetElementPtr(BaseValueTy, array, IndexValues);
    }
  } else if ((fromKind == TY_CHAR || fromKind == TY_SHORT)
      && toKind == TY_INT ) {
    if (to->is_unsigned) {
      target = Builder->CreateZExt(Base, targetTy);
    } else {
      target = Builder->CreateSExt(Base, targetTy);
    }
  } else if (fromKind == TY_LONG && toKind == TY_INT) {
    target = Builder->CreateTrunc(Base, targetTy);
  } else if (fromKind == TY_INT && toKind == TY_CHAR) {
    target = Builder->CreateTrunc(Base, targetTy);
  } else if (fromKind == TY_BOOL && toKind == TY_INT) {
    target = Builder->CreateZExt(Base, targetTy);
  } else if (fromKind == TY_INT && toKind == TY_BOOL) {
    target = Builder->CreateCmp(llvm::CmpInst::ICMP_NE, Base, Builder->getInt32(0));
  }
  return target;
}

static std::vector<llvm::Value *> push_args(Node *node) {
  std::vector<llvm::Value *> ArgsV;
  for (Node *arg = node->args; arg; arg = arg->next) {
    llvm::Value *v = gen_expr(arg);
    ArgsV.push_back(v);
  }
  return ArgsV;
}

static llvm::Value *genCast(Node *node) {
  Type *fromType = node->lhs->ty;
  std::string fromTypeStr = ctypeKindString(fromType->kind);
  Type *toType = node->ty;
  std::string toTypeStr = ctypeKindString(toType->kind);
  output << buildSeperator(stmt_level, "  ");
  output << "fromType: " << fromTypeStr << " toType: " << toTypeStr << std::endl;
  llvm::Value *V = nullptr;
  if (fromType->kind == TY_LONG && toType->kind == TY_PTR) {
    V = gen_expr(node->lhs->lhs);
  } else {
    V = gen_expr(node->lhs);
    V = cast(V, fromType, toType);
  }
  return V;
}

static llvm::Value *gen_get_ptr(Node *node, llvm::Value *baseAddr, llvm::Value *offset) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_get_ptr: " + ctypeKindString(node->ty->kind)) << std::endl;
  output << buildSeperator(cur_level, "gen_get_ptr base: " + ctypeKindString(node->ty->base->kind)) << std::endl; 
  llvm::Value *target = nullptr;
  llvm::Type *origPtrTy = nullptr;
  std::vector<llvm::Value *> IndexValues;
  if (node->ty->kind == TY_ARRAY) {
    IndexValues.push_back(getOffset(0));
    origPtrTy = yuc2LLVMType(node->ty);
  } else {
    origPtrTy = yuc2LLVMType(node->ty->base);
  }
  IndexValues.push_back(offset);
  target = Builder->CreateInBoundsGEP(origPtrTy, baseAddr, IndexValues);
  stmt_level--;

  return target;
}

static llvm::Value *gen_add_2(Node *node,
    llvm::Value *operandL, llvm::Value *operandR) {
  llvm::Value *V = nullptr;
  if (node->ty->kind == TY_PTR || node->ty->kind == TY_ARRAY) {
    V = gen_get_ptr(node->lhs->lhs, operandL, operandR);
  } else if (node->ty->is_unsigned) {
    V = Builder->CreateNUWAdd(operandL, operandR);
  } else {
    V = Builder->CreateNSWAdd(operandL, operandR);
  }
  return V;
}

static void genStore(llvm::Value *V, llvm::Value *Addr) {
  Builder->CreateStore(V, Addr);
}

static llvm::Value *gen_add(Node *node) {
  llvm::Value *operandL, *operandR;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);
  return gen_add_2(node, operandL, operandR);
}

static llvm::Value *gen_LValue(Node *node) {
  int cur_level = ++stmt_level;
  llvm::Value *LValue;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(cur_level, "gen_LValue start:" + kindStr) << std::endl;
  switch(node->kind) {
    case NodeKind::ND_VAR:
      Obj *var = node->var;
      char *name = var->name;
      if (var->is_static) {
        LValue = TheModule->getNamedGlobal(name);
      } else if (var->is_local) {
        LValue = getScopeVar(var);
      } else {
        LValue = TheModule->getNamedGlobal(name);
      }
      break;
  }
  --stmt_level;
  output << buildSeperator(cur_level, "gen_LValue end") << std::endl;
  return LValue;
}


static llvm::Value *gen_postfix(Node *node, bool isInc) {
  llvm::Value *operandL, *operandR;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);

  llvm::Value *sum = gen_add_2(node, operandL, operandR);
  llvm::Value *targetAdd = gen_LValue(node->lhs);

  genStore(sum, targetAdd);
  return operandL;
}

static llvm::Value *gen_prefix(Node *node, bool isInc) {
  llvm::Value *operandL, *operandR;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);

  llvm::Value *sum = gen_add_2(node, operandL, operandR);
  llvm::Value *targetAdd = getScopeVar(node->lhs->var);

  genStore(sum, targetAdd);
  return sum;
}


static llvm::Value *gen_assign(Node *node) {
  llvm::Value *operandL, *operandR;
  operandL = gen_LValue(node->lhs);
  operandR = gen_expr(node->rhs);
  genStore(operandR, operandL);
  return operandL;
}

static llvm::Value *gen_expr(Node *node) {
  int cur_level = ++stmt_level;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  Type* nodeType = node->ty;
  std::string typeStr = nodeType ? ctypeKindString(nodeType->kind) : "";
  output << buildSeperator(cur_level, "gen_expr start, kind:" + kindStr + " type:" + typeStr) << std::endl; 
  llvm::Value *V = nullptr;
  if (nodeType == nullptr) {
    return V;
  }
  if (kind == ND_NULL_EXPR) {
    return V;
  }
  cur_level++;
  llvm::Value *casted = nullptr;
  llvm::Value *operandL, *operandR;
  TypeKind typeKind = nodeType->kind;
  llvm::CmpInst::Predicate predicate;
  switch(kind) {
    case ND_NULL_EXPR:
      break;
    case ND_COMMA:
      gen_expr(node->lhs);
      V = gen_expr(node->rhs);
      break;
    case ND_COND:
      output << buildSeperator(cur_level, "ND_COND:") << node->kind << std::endl;
      break;
    case ND_ASSIGN:
      V = gen_assign(node);
      break;
    case ND_NUM:
      output << buildSeperator(cur_level, "ND_NUM: " + std::to_string(node->val)) << std::endl; 
      V = llvm::ConstantInt::get(nodeType->array_index ? Builder->getInt64Ty() : yuc2LLVMType(nodeType), node->val);;
      break;
    case ND_MEMZERO:
      output << buildSeperator(cur_level, "ND_MEMZERO:") << node->kind << std::endl;
      break;
    case ND_VAR:
      V = gen_RValue(node);
      break;
    case ND_DEREF:
      V = gen_expr(node->lhs);
      if (node->ty->kind != TY_ARRAY && node->lhs->ty->kind == TY_PTR) {
        V = load(node->ty, V);
      }
      break;
    case ND_ADDR:
      V = gen_addr(node->lhs);
      break;
    case ND_CAST:
      V = genCast(node);
      break;
    case ND_POST_INC:
      V = gen_postfix(node, true);
      break;
    case ND_POST_DEC:
      V = gen_postfix(node, false);
      break;
    case ND_PREFIX_INC:
      V = gen_prefix(node, true);
      break;
    case ND_PREFIX_DEC:
      V = gen_prefix(node, false);
      break;
    case ND_ADD:
      V = gen_add(node);
      break;
    case ND_SUB:
      operandL = gen_expr(node->lhs);
      operandR = gen_expr(node->rhs);
      if (node->ty->is_unsigned) {
        V = Builder->CreateNUWSub(operandL, operandR);
      } else {
        V = Builder->CreateNSWSub(operandL, operandR);
      }
      break;
    case ND_MUL:
      operandL = gen_expr(node->lhs);
      operandR = gen_expr(node->rhs);
      if (node->ty->is_unsigned) {
        V = Builder->CreateNUWAdd(operandL, operandR);
      } else {
        V = Builder->CreateNSWAdd(operandL, operandR);
      }
      break;
    case ND_DIV:
      operandL = gen_expr(node->lhs);
      operandR = gen_expr(node->rhs);
      if (node->ty->is_unsigned) {
        V = Builder->CreateUDiv(operandL, operandR);
      } else {
        V = Builder->CreateSDiv(operandL, operandR);
      }
      break;
    case ND_FUNCALL: {
      std::vector<llvm::Value *> ArgsV = push_args(node);
      std::string Callee = node->lhs->var->name;
      output << buildSeperator(cur_level, Callee) << std::endl;
      llvm::Function *CalleeF = getFunction(Callee);
      V = Builder->CreateCall(CalleeF, ArgsV, "");
      break;
    }
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      operandL = gen_expr(node->lhs);
      operandR = gen_expr(node->rhs);

      if (kind == ND_EQ) {
        if (isFloatTypeKind(typeKind)) {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::FCMP_UEQ;
          } else {
            predicate = llvm::CmpInst::Predicate::FCMP_OEQ;
          }
        } else {
          predicate = llvm::CmpInst::Predicate::ICMP_EQ;
        }
      } else if (kind == ND_NE) {
        if (isFloatTypeKind(typeKind)) {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::FCMP_UNE;
          } else {
            predicate = llvm::CmpInst::Predicate::FCMP_ONE;
          }
        } else {
          predicate = llvm::CmpInst::Predicate::ICMP_NE;
        }
      } else if (kind == ND_LT) {
        if (isFloatTypeKind(typeKind)) {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::FCMP_ULT;
          } else {
            predicate = llvm::CmpInst::Predicate::FCMP_OLT;
          }
        } else {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::ICMP_ULT;
          } else {
            predicate = llvm::CmpInst::Predicate::ICMP_SLT;
          }
        }
      } else if (kind == ND_LE) {
        if (isFloatTypeKind(typeKind)) {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::FCMP_ULE;
          } else {
            predicate = llvm::CmpInst::Predicate::FCMP_OLE;
          }
        } else {
          if (nodeType->is_unsigned) {
            predicate = llvm::CmpInst::Predicate::ICMP_ULE;
          } else {
            predicate = llvm::CmpInst::Predicate::ICMP_SLE;
          }
        }
      }

      V = Builder->CreateCmp(predicate, operandL, operandR);
      break;
    default:
      break;
  }
  stmt_level--;
  cur_level--;
  output << buildSeperator(cur_level, "gen_expr end") << std::endl;
  return V;
}

static void gen_if(Node *node) {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  llvm::Value *condValue = gen_expr(node->cond);
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(CGM().getLLVMContext(), "", TheFunction);
  llvm::BasicBlock *ElseBB = node->els ? llvm::BasicBlock::Create(CGM().getLLVMContext()) : nullptr;
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(CGM().getLLVMContext());

  Builder->CreateCondBr(condValue, ThenBB, ElseBB ? ElseBB : MergeBB);
  
  Builder->SetInsertPoint(ThenBB);
  gen_stmt(node->then);
  Builder->CreateBr(MergeBB);
  ThenBB = Builder->GetInsertBlock();

  if (ElseBB) {
    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);
    gen_stmt(node->els);
    Builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder->GetInsertBlock();
  }

  // Emit merge block.
  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder->SetInsertPoint(MergeBB);
}

static void gen_return(Node *node) {
  if (!node->lhs) {
    return;
  }
  llvm::Value *tmpV = gen_expr(node->lhs);
  if (allocatedRetValue) {
    Builder->CreateStore(tmpV, allocatedRetValue);
  } else{
    retValue = tmpV;
  }
}

static void gen_stmt(Node *node) {
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(level, "gen_stmt start:") << " "<< cur_count << std::endl;
  output << buildSeperator(level, kindStr) << std::endl;
  llvm::Value *tmpV;
  switch(kind) {
    case ND_IF:
      gen_if(node);
      break;
    case ND_BLOCK: // 32
      for (Node *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      break;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      break;
    case ND_RETURN:
      gen_return(node);
      break;
    default:
      output << buildSeperator(level+1, kindStr) << std::endl;
      break;
  }
  output << buildSeperator(level, "gen_stmt end ") <<  cur_count << std::endl;
  --stmt_level;
}


static llvm::ArrayType *yuc2ArrayType(Type *ctype) {
  llvm::Type *base = yuc2LLVMType(ctype->base);
  int array_len = ctype->array_len;
  llvm::ArrayType *type = llvm::ArrayType::get(base, array_len);
  return type;
}

static llvm::Type *yuc2PointerType(Type *ctype) {
  // output << "yuc2PointerType baseType ";
  llvm::Type *base = yuc2LLVMType(ctype->base);
  llvm::Type *type = llvm::PointerType::get(base, 0);
  return type;
}

static int getMemberCount(Type *ctype) {
   int memberCount = 0;
  for (Member *member = ctype->members; member; member = member->next) {
    memberCount++;
  }
  return memberCount;	
}

static llvm::StructType *yuc2StructType(Type *ctype) {
  //output << "yuc2StructType" << std::endl;
  llvm::SmallVector<llvm::Type *, 16> Types;

  int memberCount = getMemberCount(ctype);
  Types.reserve(memberCount);
  Member *member = ctype->union_field;
  llvm::StructType *type;
  int DesiredSize = ctype->size;
  output << "yuc2StructType DesiredSize:" << DesiredSize << std::endl;
  // union
  if (member) {
    int Size = member->ty->size;
    output << "union member idx: " << member->idx << " size: " << Size << std::endl;
    
    Types.push_back(yuc2LLVMType(member->ty));
    if (DesiredSize > Size) {
      int PadSize = DesiredSize - Size;
      output << "PadSize: " << PadSize << std::endl;
      llvm::Type *paddingType = getPaddingType(PadSize);
      Types.push_back(paddingType);
    }

    if (member->idx) {
      type = llvm::StructType::get(*TheContext, Types, false);
    } else {
      type = llvm::StructType::create(*TheContext, Types, "", false);
    }
    output << "final type: " << Types.size() << std::endl;
    return type;
  }
  // struct
  for (Member *member = ctype->members; member; member = member->next) {
    Types.push_back(yuc2LLVMType(member->ty));
  }
  if (ctype->is_typedef) {
    type = llvm::StructType::get(*TheContext, Types, false);
  } else {
    type = llvm::StructType::create(*TheContext, Types, "", false);
  }
  return type;
}

void addRecordTypeName(Type *ctype, llvm::StructType *Ty, llvm::StringRef suffix) {
  llvm::SmallString<256> TypeName;
  llvm::raw_svector_ostream OS(TypeName);
  std::string kindName = ctypeKindString(ctype->kind);
  OS << kindName << ".";

  OS << "anon";
  if (!suffix.empty()) {
    OS << suffix;
  }
  Ty->setName(OS.str());
}

static llvm::StructType *ConvertRecordDeclType(Type *ctype) {
  llvm::StructType *DesiredType = yuc2StructType(ctype);
  addRecordTypeName(ctype, DesiredType, "");
  return DesiredType;
}

static llvm::Type *yuc2LLVMType(Type *ctype) {
  // output << "yuc2LLVMType: " << ctypeKindString(ctype->kind)  << std::endl;
  llvm::Type *type;
  switch (ctype->kind) {
    case TY_BOOL:
      type = Builder->getInt1Ty();
      break;
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
    case TY_PTR:
     	type = yuc2PointerType(ctype);
      break;
    case TY_ARRAY:
      type = yuc2ArrayType(ctype);
      break;
    case TY_UNION:
    case TY_STRUCT:
      type = ConvertRecordDeclType(ctype);
      break;
    default:
      type = Builder->getInt32Ty();
      break;
  }
  return type;
}

static llvm::GlobalVariable *
GenerateStringLiteral(llvm::Constant *C, llvm::GlobalValue::LinkageTypes LT,
                      IRGenModule &CGM, llvm::StringRef GlobalName,
                      int Alignment) {
  unsigned AddrSpace = 0;
  llvm::Module &M = CGM.getModule();
  // Create a global variable for this string
  auto *GV = new llvm::GlobalVariable(
      M, C->getType(), true, LT, C, GlobalName,
      nullptr, llvm::GlobalVariable::NotThreadLocal, AddrSpace);
  GV->setAlignment(llvm::MaybeAlign(Alignment));
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  if (GV->isWeakForLinker()) {
    GV->setComdat(M.getOrInsertComdat(GV->getName()));
  }
  return GV;
}

IRGenModule::IRGenModule(llvm::Module &M)
  : TheModule(M), VMContext(M.getContext()) {
  // Initialize the type cache.
  llvm::LLVMContext &LLVMContext = M.getContext();
}

IRGenModule::~IRGenModule() {}

llvm::GlobalVariable *IRGenModule::GetAddrOfConstantCString(const std::string &Str, const char *GlobalName) {
  if (strLiteralCache[Str]) {
    return strLiteralCache[Str];
  }

  llvm::StringRef StrWithNull(Str.c_str(), Str.size() + 1);
  llvm::Constant *C =
      llvm::ConstantDataArray::getString(getLLVMContext(), StrWithNull, false);
  int Alignment = 1;
  // Get the default prefix if a name wasn't specified.
  if (!GlobalName)
    GlobalName = ".str";
  // Create a global variable for this.
  auto GV = GenerateStringLiteral(C, llvm::GlobalValue::PrivateLinkage, *this,
                                  GlobalName, Alignment);
  strLiteralCache[Str] = GV;
  return GV;
}

static std::unique_ptr<IRGenModule> ModuleBuilder;

static IRGenModule &CGM() {
  return *ModuleBuilder;
}

static uint64_t read_buf(char *buf, int sz) {
  if (sz == 1)
    return *buf;
  if (sz == 2)
    return *(uint16_t *)buf;
  if (sz == 4)
    return *(uint32_t *)buf;
  if (sz == 8)
    return *(uint64_t *)buf;
  return *buf;
}

llvm::Constant *EmitArrayInitialization(llvm::ArrayType *Ty, Type *ArrayType, char *buf, int offset, Relocation *rel) {
  llvm::SmallVector<llvm::Constant *, 16> Elts;
  unsigned NumElements = ArrayType->array_len;
  Elts.reserve(NumElements);
  int sz = ArrayType->base->size;

  output << "EmitArrayInitialization： " << NumElements 
    << " size: " <<  sz
    << std::endl;
  
  llvm::Type *CommonElementType = Ty->getElementType();
  Type *baseTy = ArrayType->base;
  llvm::Constant *C = nullptr;
  if (!rel) {
     for(int i = 0; i < NumElements; i++) {
      C = buffer2Constants(CommonElementType, baseTy, NULL, buf, offset + sz * i);
      Elts.push_back(C);
    }   
  } else {
    int i = 0;
    while(rel) {
      output << "label: " << *rel->label << std::endl;
      C = buffer2Constants(CommonElementType, baseTy, rel, buf, offset + sz * i);
      Elts.push_back(C);
      rel = rel->next;
      i++;
    } 
  }

  return llvm::ConstantArray::get(
        llvm::ArrayType::get(CommonElementType, NumElements), Elts);
 }
 
llvm::Constant *EmitRecordInitialization(llvm::StructType *Ty, Type *ctype, char *buf, int offset) {
  llvm::SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = getMemberCount(ctype);

  Elements.reserve(NumElements);
  output << "EmitRecordInitialization NumElements： " << NumElements
    << std::endl;
  int index = 0; 
  for (Member *member = ctype->members; member; member = member->next) {
    // Type *Ty = yuc2LLVMType(member->ty);
    llvm::Type *memberTy = Ty->getTypeAtIndex(index++);
    llvm::Constant *constantValue = buffer2Constants(memberTy, member->ty, NULL, buf, offset + member->offset);
    Elements.push_back(constantValue);
  }

  return llvm::ConstantStruct::get(Ty, Elements);
}  

llvm::Constant *EmitUnionInitialization(llvm::StructType *Ty, Type *ctype, char *buf, int offset) {
  int DesiredSize = ctype->size;
  int AlignedSize = ctype->align; 
  llvm::SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = getMemberCount(ctype);
  output << "EmitUnionInitialization NumElements： " << NumElements 
    << " DesiredSize: " <<  DesiredSize
    << " AlignedSize: " <<  AlignedSize 
    << std::endl;
  Elements.reserve(NumElements);
  bool Packed = false;
  Member *member = ctype->union_field;
  if (member) {
    int Size = member->ty->size;
    output << "EmitUnionInitialization union Size " << Size << std::endl;
    llvm::Type *Ty = yuc2LLVMType(member->ty);
    llvm::Constant *constantValue = buffer2Constants(Ty, member->ty, NULL, buf, offset + member->offset);
    Elements.push_back(constantValue);
    if (DesiredSize > Size) {
      Elements.push_back(getPadding(DesiredSize - Size));
    }
  }
  llvm::StructType *STy = llvm::ConstantStruct::getTypeForElements(CGM().getLLVMContext(), Elements, Packed);
  return llvm::ConstantStruct::get(STy, Elements);
}

/// Return the value offset.
static llvm::Constant *getOffset(long offset) {
  return llvm::ConstantInt::get(Builder->getInt64Ty(), offset);
}

/// Apply the value offset to the given constant.
static llvm::Constant *applyOffset(llvm::Constant *C, long offset) {
  if (!offset)
    return C;

  llvm::Type *origPtrTy = C->getType();
  unsigned AS = origPtrTy->getPointerAddressSpace();
  llvm::Type *charPtrTy = Builder->getInt8Ty()->getPointerTo(AS);
  C = llvm::ConstantExpr::getBitCast(C, charPtrTy);
  C = llvm::ConstantExpr::getGetElementPtr(Builder->getInt8Ty(), C, getOffset(offset));
  C = llvm::ConstantExpr::getPointerCast(C, origPtrTy);
  return C;
}

llvm::Constant *EmitPointerInitialization(llvm::PointerType *Ty, Type *ctype, char *buf, int offset, Relocation *rel) {
  output << "EmitPointerInitialization: size: " << ctype->size << std::endl;

  llvm::Constant *Base = nullptr;
  llvm::Type *BaseValueTy = nullptr;

  llvm::SmallVector<unsigned, 8> Indices;
  llvm::SmallVector<llvm::Constant*, 8> IndexValues;
  // Initialize the stack.
  Indices.push_back(0);
  IndexValues.push_back(nullptr);

  if (rel) {
    long addend = rel->addend;
    Indices.push_back(addend);
    IndexValues.push_back(nullptr);
    char **label = rel->label;
    char *name = *label;
    output << " addend: " << addend << " name:" << name << std::endl;
    llvm::GlobalVariable *global;
    std::string nameStr = name;
    if (isAnnonVar(nameStr)) {
      const std::string &Str = annonInitData[nameStr];
      global = CGM().GetAddrOfConstantCString(Str, nullptr);
      output << " annonGlobalVar: " << global->isConstant() << std::endl;
    } else {
      global = TheModule->getGlobalVariable(name);
    }
    
    llvm::Constant *constant = global->getInitializer();
    Base = global;
    BaseValueTy = constant->getType();
    IndexValues[0] = llvm::ConstantInt::get(Builder->getInt32Ty(), Indices[0]);

    TypeKind baseTypeKind = ctype->base->kind;
    output << " base kind:" << ctypeKindString(baseTypeKind);
    if (addend == 0) {
      if (BaseValueTy->isIntegerTy ()) {
        return Base;
      }
    }

    if (addend >= 0) {
      llvm::Constant *value = applyOffset(Base, addend);
      return llvm::ConstantExpr::getPointerCast(value, Ty);
      for (size_t i = Indices.size() - 1; i != size_t(0); --i) {
        int indice = Indices[i];
        output << " indice: " << indice;

        llvm::Constant *indVal = nullptr;
        if (indice) {
          indVal = getOffset(indice);
        } else {
          indVal = llvm::ConstantInt::get(Builder->getInt32Ty(), indice);
        }
        IndexValues[i] = getOffset(4);
      }
      llvm::Constant *location = llvm::ConstantExpr::getInBoundsGetElementPtr(BaseValueTy, Base, IndexValues);
      return location;
    }

    IndexValues[1] = getOffset(addend);
    llvm::Constant *location =
      llvm::ConstantExpr::getGetElementPtr(BaseValueTy, Base, IndexValues);
    return location;
  }  
} 

llvm::Constant *EmitIntegerInitialication(llvm::Type *destTy, Type *IntType, char *buf, int offset, Relocation *rel) {
  int size = IntType->size;
  if (!buf) {
    return llvm::ConstantInt::get(destTy, 0);
  }
  if (!rel) {
    return llvm::ConstantInt::get(destTy, read_buf(buf + offset, size));
  }
  output << "label: " << *rel->label << std::endl;
  char **label = rel->label;
    char *name = *label;
  llvm::GlobalVariable *global = TheModule->getGlobalVariable(name);
  llvm::Constant *value = global->getInitializer();
  return llvm::ConstantExpr::getPtrToInt(global, destTy);
}

static llvm::Constant *buffer2Constants(llvm::Type *varType, Type *ctype, Relocation *rel, char *buf, int offset) {
  llvm::Constant *constant;
  int size = ctype->size;
  switch(ctype->kind) {
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
      constant = EmitIntegerInitialication(varType, ctype, buf, offset, rel);
      break;
    case TY_ARRAY:
      constant = EmitArrayInitialization(static_cast<llvm::ArrayType *>(varType), ctype, buf, offset, rel);
      break;
    case TY_UNION:
      constant = EmitUnionInitialization(static_cast<llvm::StructType *>(varType), ctype, buf, offset);
      break;
    case TY_STRUCT:
      constant = EmitRecordInitialization(static_cast<llvm::StructType *>(varType), ctype, buf, offset);
      break; 
    case TY_PTR:
      constant = EmitPointerInitialization(static_cast<llvm::PointerType *>(varType), ctype, buf, offset, rel);
      break;
    default:
      constant = Builder->getInt32(-1024);
      break;
  }
  return constant;
}

static llvm::Constant *yuc2Constants(llvm::Type *Ty, Obj *gvarNode) {
  llvm::Constant *constant = buffer2Constants(Ty, gvarNode->ty, gvarNode->rel, gvarNode->init_data, 0);
  return constant;
}

static llvm::GlobalValue::LinkageTypes yuc2LinkageType(Obj *yucNode) {
  llvm::GlobalValue::LinkageTypes ret = llvm::GlobalValue::LinkageTypes::ExternalLinkage;
  if (yucNode->is_static) {
    ret = llvm::GlobalValue::LinkageTypes::InternalLinkage;
  }

  return ret;
}

static void InitializeModule(const std::string &moduleName) {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>(moduleName, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  ModuleBuilder = std::make_unique<IRGenModule>(*TheModule);
}

void processAnnonVar(Obj *ast) {
	if (ast) {
		processAnnonVar(ast->next);
		std::string name = ast->name;
		annonInitData[name] = ast->init_data;
	}
}

static llvm::GlobalVariable *createGlobalVar(Obj *yucNode) {
	std::string name = yucNode->name;

	Type *ctype = yucNode->ty;
	if (ctype->kind == TY_FUNC) {
		return nullptr;
	}
  output << "createGlobalVar name:" << name;
	output << " kind:" << ctypeKindString(ctype->kind);
  if (ctype->base) {
    output << " base " << ctypeKindString(ctype->base->kind);
  }
  if (ctype->origin) {
    output << " origin " << ctypeKindString(ctype->origin->kind);
  }
  output << std::endl;
  llvm::Type *DesiredType = yuc2LLVMType(ctype);
  TheModule->getOrInsertGlobal(name, DesiredType);
  llvm::GlobalVariable *gVar = TheModule->getNamedGlobal(name);
  gVar->setAlignment(llvm::MaybeAlign(yucNode->align));
  if (!yucNode->is_static) {
  	gVar->setDSOLocal(true);
  }

  llvm::Constant *initializer = yuc2Constants(DesiredType, yucNode);
  putGlobalVar(yucNode, initializer);
  gVar->setInitializer(initializer);
  gVar->setLinkage(yuc2LinkageType(yucNode));
  return gVar;
}

static void emit_data(Obj *ast) {
  if (!ast) {
    return;
  }
  emit_data(ast->next);
  createGlobalVar(ast);
}

static llvm::FunctionType * buildFunctionType(Obj *funcNode) {
  std::vector<llvm::Type *> types;
  Type *funcType = funcNode->ty;
  for (Type *paramType = funcType->params; paramType; paramType = paramType->next) {
    llvm::Type *type = yuc2LLVMType(paramType);
    types.push_back(type);
  }

  llvm::Type *RetTy = yuc2LLVMType(funcType->return_ty);
  bool isVarArg = funcType->is_variadic;
  if (types.empty()) {
    isVarArg = false;
  }
  llvm::FunctionType *functionType = llvm::FunctionType::get(RetTy, types, isVarArg);
  return functionType;
}

static void ConstructAttributeList(llvm::AttributeList &AttrList) {
  llvm::AttrBuilder FuncAttrs(CGM().getLLVMContext());
  llvm::AttrBuilder RetAttrs(CGM().getLLVMContext());
  llvm::AttrBuilder Attrs(CGM().getLLVMContext());

  FuncAttrs.addAttribute(llvm::Attribute::SExt);
  RetAttrs.addAttribute(llvm::Attribute::ZExt);

  llvm::SmallVector<llvm::AttributeSet, 4> ArgAttrs(1);
  Attrs.addAttribute(llvm::Attribute::ZExt);
  Attrs.addAttribute(llvm::Attribute::NoUndef);
  
  ArgAttrs[0] =
            llvm::AttributeSet::get(CGM().getLLVMContext(), Attrs);

  AttrList = llvm::AttributeList::get(
      CGM().getLLVMContext(), llvm::AttributeSet::get(CGM().getLLVMContext(), FuncAttrs),
      llvm::AttributeSet::get(CGM().getLLVMContext(), RetAttrs), ArgAttrs);
}

static void SetLLVMFunctionAttributes(llvm::Function *F) {
  llvm::AttributeList PAL;
  ConstructAttributeList(PAL);
  F->setAttributes(PAL);
}

static llvm::Function *createFunc(Obj *funcNode) {
  llvm::FunctionType *funcType = buildFunctionType(funcNode);
  std::string funcName = funcNode->name;
  llvm::GlobalValue::LinkageTypes linkageType = yuc2LinkageType(funcNode);
  llvm::Function *fooFunc = llvm::Function::Create(funcType, linkageType, funcName, TheModule.get());
  if(!funcNode->is_static) {
    fooFunc->setDSOLocal(true);
  }
  return fooFunc;
}

static void setFuncArgs(llvm::Function *Func, std::vector<std::string> FuncArgs) {
  unsigned Idx = 0;
  llvm::Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, ++Idx) {
      AI->setName(FuncArgs[Idx]);
    }
}

static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                          const std::string &VarName,
                                          llvm::Type *Ty) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                 TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Ty, 0,
                           VarName.c_str());
}

static llvm::AllocaInst *CreateMainEntryBlockAlloca(llvm::Function *TheFunction,
                                          const std::string &VarName) {
  return CreateEntryBlockAlloca(TheFunction, VarName, llvm::Type::getInt32Ty(*TheContext));
}

static void FinishFunction(llvm::Function *Func, Obj *funcNode) {
  llvm::Type *returnType = Func->getReturnType();
  llvm::Value *finalRetValue = retValue;
  if (allocatedRetValue) {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *ReturnBB = llvm::BasicBlock::Create(CGM().getLLVMContext());
    Builder->CreateBr(ReturnBB);

    TheFunction->getBasicBlockList().push_back(ReturnBB);
    Builder->SetInsertPoint(ReturnBB);
    finalRetValue = Builder->CreateLoad(returnType, allocatedRetValue);   
  }
  Builder->CreateRet(finalRetValue);

  retValue = nullptr;
  allocatedRetValue = nullptr;
  clearScopeVars();
}

static void prepareLocals(llvm::Function *Func, Obj *funcNode) {
  if (funcNode->retCount > 1) {
    llvm::Type *returnType = Func->getReturnType();
    allocatedRetValue = Builder->CreateAlloca(returnType, nullptr);
  }

  std::vector<Obj *> locals;
   for (Obj *local = funcNode->locals; local; local = local->next) {
    output << "start local name: " << local->name << std::endl;
    locals.push_back(local);
  }
  // reverse(locals.begin(), locals.end());

  static std::map<int, llvm::Value*> LocallAddress;
  for (std::vector<Obj *>::iterator iter = locals.begin();
    iter != locals.end(); iter++) {
    Obj *local = (*iter);
    std::string varName = local->name;
    output << "local name: " << varName
      << " offset " << local->offset << std::endl;
    if (varName == "__alloca_size__" || varName == "__va_area__") {
      continue;
    }
    llvm::Type *localType = getTypeWithArg(local->ty);
    llvm::Value *localAddr = Builder->CreateAlloca(localType, nullptr);
    putScopeVar(local, localAddr);
    LocallAddress[local->offset] = localAddr;
  }

  // reverse args
  std::vector<Obj *> args;
  for (Obj *param = funcNode->params; param; param = param->next) {
    output << "param name: " << param->name << std::endl;
    args.push_back(param);
  }
  // reverse(args.begin(), args.end());

  // store args
  std::vector<Obj *>::iterator arg_iter = args.begin();
  llvm::Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, arg_iter++) {
    Obj *arg = (*arg_iter);
    output << "arg name: " << arg->name
      << " offset " << arg->offset << std::endl;
    llvm::Value *argAddr = LocallAddress[arg->offset];
    TypeKind typeKind = arg->ty->kind;
    llvm::Value *fnArg = AI;
    if (typeKind == TY_BOOL) {
      fnArg = Builder->CreateZExt(AI, Builder->getInt8Ty());
    }
    Builder->CreateStore(fnArg, argAddr);
  }
}

static void StartFunction(llvm::Function *Fn, Obj *funcNode) {
  const char *name = Fn->getName().data();
  output << "\nStartFunction " << name << std::endl;
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*TheContext, "", Fn);
  Builder->SetInsertPoint(entry);

  llvm::AllocaInst *defaultAlloca = nullptr;
  if (!strcmp(name, "main")) {
    defaultAlloca = CreateMainEntryBlockAlloca(Fn, "");
  }
  prepareLocals(Fn, funcNode);

  if (defaultAlloca) {
    Builder->CreateStore(Builder->getInt32(0), defaultAlloca);
  }
}

static void buildFunctionBody(llvm::Function *Func, Obj *funcNode) {
  output << "buildFunctionBody " << std::endl;
  gen_stmt(funcNode->body);
}

static void buildFunction(Obj *funcNode) {
  if (!funcNode || funcNode->ty->kind != TY_FUNC) {
    return;
  }
  llvm::Function *fooFunc = createFunc(funcNode);
  SetLLVMFunctionAttributes(fooFunc);
  if (!funcNode->is_definition) {
    return;
  }
 StartFunction(fooFunc, funcNode);
 buildFunctionBody(fooFunc, funcNode);
 FinishFunction(fooFunc, funcNode);
 llvm::verifyFunction(*fooFunc);
}

static void emit_text(Obj *fn) {
  if (!fn) {
    return;
  }
  emit_text(fn->next);
  buildFunction(fn);
}

void gen_ir(Obj *prog, const std::string &filename) {
	Obj *annonP, *namedP;
	Obj **annon = &annonP, **named = &namedP;
	for (Obj *cur = prog; cur; cur = cur->next) {
		std::string name = cur->name;
    output << "gen ir: " << name << std::endl;
		if (isAnnonVar(name)) {
			*annon = cur;
			annon = &cur->next;
		} else {
			*named = cur;
			named = &cur->next;
		}
	}

	*annon = nullptr;
	*named = nullptr;
	
	InitializeModule(filename);
	processAnnonVar(annonP);
	emit_data(namedP);
  emit_text(namedP);
	TheModule->dump();
	output << "yuc end" << std::endl;
}

