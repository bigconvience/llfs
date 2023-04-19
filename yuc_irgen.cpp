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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/InlineAsm.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#define DUMP_SCOPE 0

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
// https://llvm.org/doxygen/IRBuilder_8h_source.html
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, char *> annonInitData;
static std::map<std::string, llvm::GlobalVariable*> strLiteralCache;
static std::map<std::string, llvm::BasicBlock*> sLabelBB;

static int stmt_level = 0;
static int stmt_count = 0;
static std::ofstream output("./test2/ast.out");
static llvm::Constant *buffer2Constants(llvm::Type *Ty, Type *ctype, Relocation *rel, char *buf, int offset);
static llvm::Constant *yuc2Constants(llvm::Type *Ty, Obj *gvarNode);
static llvm::GlobalVariable *createGlobalVar(Obj *yucNode);
static llvm::Type *yuc2LLVMType(Type *ctype);
static llvm::Type *getTypeForArg(Type *ctype);
static llvm::Constant *getOffset(long offset);
static llvm::BasicBlock *createBasicBlock();

static llvm::Value *gen_expr(Node *node);
static void gen_stmt(Node *node);
static llvm::Value *gen_addr(Node *node);
static llvm::Value *gen_get_ptr(Node *node, llvm::Value *baseAddr, llvm::Value *offset);
static llvm::Value *gen_member(Node *node);
static llvm::Value *gen_member_addr(Node *node);
static llvm::Value *gen_get_addr(llvm::Value *baseAddr);
static llvm::Value *gen_not(Node *node);
static llvm::Value *gen_cond(Node *node);
static llvm::Value *gen_comma(Node *node);
static llvm::Value *gen_comma_addr(Node *node);
static void gen_label(Node *node);
static llvm::Value* cast(llvm::Value *Base, Type *from, Type *to);
static void gen_branch(llvm::BasicBlock *target);
static void gen_basicblock(llvm::BasicBlock *target);
static llvm::Value *gen_asm(Node *node);

static std::map<Obj *, llvm::Value*> scopeVars;
static std::map<Obj *, llvm::Constant*> globalVars;
static std::map<Type *, llvm::StructType*> tagToType;
static llvm::Value *gen_number(Node *node);

static void addRecordTypeName(Type *ctype, llvm::StructType *Ty);
static llvm::Type *emit_pointer_type(Type *ctype);
static int getMemberCount(Type *ctype);
static llvm::StructType *ConvertRecordDeclType(Type *ctype);
static llvm::Constant *gen_literal(Node *node);
static llvm::GlobalValue::LinkageTypes yuc2LinkageType(Obj *yucNode);
std::string get_struct_name(Type *ctype, bool packed);
static llvm::StructType *create_packed_struct_type(llvm::StructType *normal_type, Type *ctype);
static llvm::StructType *get_init_struct_type(Type *ctype);
static llvm::Constant *init_struct(llvm::StructType *type, Type *ctype, char *buf);
static llvm::Value *gen_assign_member(llvm::Value *member_addr, llvm::Value *rhs, Node *lhs);

enum { B8, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80, PTR };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_BOOL:
    return B8;
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  case TY_PTR:
    return PTR;
  }
  return I64;
}

// 求8的倍数
int align_to_eight(int n) {
  return ((n - 1) / 8 + 1);
}

static llvm::Type *get_int_n_ty(int size) {
  return Builder->getIntNTy(size * 8);
}

static bool is_bool_value(llvm::Value *V) {
  return V->getType() == Builder->getInt1Ty();
}

// ptr to i8* 
static llvm::Value *uniform_address(llvm::Value *V) {
  llvm::Type * type = llvm::PointerType::get(Builder->getInt8Ty(), 0);
  return Builder->CreateBitCast(V, type);
}

// float to bool
static llvm::Value *fpToBool(llvm::Value *V) {
  llvm::Value *operand;
  llvm::Value *Zero = llvm::Constant::getNullValue(V->getType());
  llvm::CmpInst::Predicate predicate = llvm::CmpInst::Predicate::FCMP_UNE;
  operand = Builder->CreateCmp(predicate, V, Zero);
  return operand;
}

// gen bool for float or int
static llvm::Value *gen_to_bool(Node *node) {
  llvm::Value *condV = gen_expr(node);

  if (is_flonum(node->ty)) {
    condV = fpToBool(condV);
  } else if (!is_bool_value(condV)) {
    condV = Builder->CreateIsNotNull(condV);
  }
  return condV;
}

// The table for type casts
static llvm::Value *i64i8(llvm::Value *V, Type *to_type) 
{
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateTrunc(V, targetTy);
};

static llvm::Value *i8i64(llvm::Value *V, Type *to_type) 
{
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateSExt(V, targetTy);
};

static llvm::Value *u8i64(llvm::Value *V, Type *to_type) 
{
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateZExt(V, targetTy);
};

// signed int to double long
static llvm::Value *i8f80(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateSIToFP(V, targetTy);
};

// unsigned int to double long
static llvm::Value *u8f80(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateUIToFP(V, targetTy);
};

// double long to signed int
static llvm::Value *f80i8(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateFPToSI(V, targetTy);
};

// double long to unsigned int
static llvm::Value *f80u8(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateFPToUI(V, targetTy);
};

static llvm::Value *ptrptr(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateBitCast(V, targetTy);
}

static llvm::Value *ptrint(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreatePtrToInt(V, targetTy);
}

static llvm::Value *intptr(llvm::Value *V, Type *to_type) {
  if (V->getType()->getIntegerBitWidth() < 64) {
    V = Builder->CreateZExt(V, Builder->getInt64Ty());
  }
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateIntToPtr(V, targetTy);
}

// float to BOOL
static llvm::Value *f64b8(llvm::Value *V, Type *to_type) {
  llvm::Value *cmp = fpToBool(V);
  return Builder->CreateZExt(cmp, Builder->getInt8Ty());
}

// int to BOOL
static llvm::Value *i64b8(llvm::Value *V, Type *to_type) {
  llvm::Value *Zero = llvm::Constant::getNullValue(V->getType());
  llvm::CmpInst::Predicate predicate = llvm::CmpInst::Predicate::ICMP_NE;
  llvm::Value *cmp = Builder->CreateCmp(predicate, V, Zero);
  return Builder->CreateZExt(cmp, Builder->getInt8Ty());
}

// BOOL to float
static llvm::Value *b8f64(llvm::Value *V, Type *to_type) {
  return V;
}

// BOOL to int
static llvm::Value *b8i64(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateZExt(V, targetTy);
}

static llvm::Value *fpext(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateFPExt(V, targetTy);
}

static llvm::Value *fptrunc(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = yuc2LLVMType(to_type);
  return Builder->CreateFPExt(V, targetTy);
}

static llvm::Value *(*cast_table[][13])(llvm::Value *, Type *) = {
  // b8    i8   i16     i32   i64     u8   u16     u32    u64,   f32    f64   f80    ptr
  {u8i64, u8i64, u8i64, u8i64, u8i64, u8i64, u8i64, u8i64, u8i64, i8f80, i8f80, i8f80, intptr}, // b8

  {i64b8, NULL, i8i64, i8i64, i8i64, NULL, i8i64, i8i64, i8i64, i8f80, i8f80, i8f80, intptr}, // i8
  {i64b8, i64i8, NULL,  i8i64, i8i64, NULL, NULL, i8i64, i8i64, i8f80, i8f80, i8f80, intptr}, // i16
  {i64b8, i64i8, i64i8, NULL,  i8i64, i64i8, i64i8, i64i8, i8i64, i8f80, i8f80, i8f80, intptr}, // i32
  {i64b8, i64i8, i64i8, i64i8, NULL,  NULL, NULL, NULL, NULL, i8f80, i8f80, i8f80, intptr}, // i64

  {i64b8, NULL, u8i64, u8i64, u8i64, NULL, NULL, NULL, NULL, u8f80, u8f80, u8f80, intptr}, // u8
  {i64b8, i64i8, NULL,  u8i64, u8i64, NULL, NULL, NULL, NULL, u8f80, u8f80, u8f80, intptr}, // u16
  {i64b8, i64i8, i64i8, NULL,  u8i64, NULL, NULL, NULL, NULL, u8f80, u8f80, u8f80, intptr}, // u32
  {i64b8, i64i8, i64i8, i64i8, NULL,  NULL, NULL, NULL, NULL, u8f80, u8f80, u8f80, intptr}, // u64

  {f64b8, f80i8, f80i8, f80i8, f80i8, f80u8, f80u8, f80u8, f80u8, NULL,  fpext, fpext, NULL}, // f32
  {f64b8, f80i8, f80i8, f80i8, f80i8, f80u8, f80u8, f80u8, f80u8, fptrunc,  NULL, fpext, NULL}, // f64
  {f80i8, f80i8, f80i8, f80i8, f80i8, f80u8, f80u8, f80u8, f80u8, fptrunc,  fptrunc, NULL, NULL}, // f80

  {NULL,  ptrint, ptrint, ptrint, ptrint, ptrint, ptrint, ptrint, ptrint, NULL, NULL, NULL, ptrptr}, // ptr
};

enum { LT, LE, GT, GE, EQ, NE };
enum { UNSIGNED, SIGNED};
enum { Int, Float };

static int getRelationId(Node *node) {
  switch(node->kind) {
  case ND_LT:
    return LT;
  case ND_LE:
    return LE;
  case ND_GT:
    return GT;
  case ND_GE:
    return GE;
  case ND_EQ:
    return EQ;
  case ND_NE:
    return NE;
  }
  return LT;
}

static int getTypeCatagory(Type *ty) {
  return is_flonum(ty) ? Float : Int;
}

static int getSignId(Type *ty) {
  return ty->is_unsigned ? UNSIGNED : SIGNED;
}

// [Relation][Signed][Integer]
static llvm::CmpInst::Predicate predicate_table[][2][2] = {
  {
    {llvm::CmpInst::Predicate::ICMP_ULT, llvm::CmpInst::Predicate::FCMP_OLT}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_SLT, llvm::CmpInst::Predicate::FCMP_OLT}, // Signed
  }, // LT
  {
    {llvm::CmpInst::Predicate::ICMP_ULE, llvm::CmpInst::Predicate::FCMP_OLE}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_SLE, llvm::CmpInst::Predicate::FCMP_OLE}, // Signed
  }, // LE
  {
    {llvm::CmpInst::Predicate::ICMP_UGT, llvm::CmpInst::Predicate::FCMP_OGT}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_SGT, llvm::CmpInst::Predicate::FCMP_OGT}, // Signed
  }, // GT
  {
    {llvm::CmpInst::Predicate::ICMP_UGE, llvm::CmpInst::Predicate::FCMP_OGE}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_SGE, llvm::CmpInst::Predicate::FCMP_OGE}, // Signed
  }, // GE
  {
    {llvm::CmpInst::Predicate::ICMP_EQ, llvm::CmpInst::Predicate::FCMP_OEQ}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_EQ, llvm::CmpInst::Predicate::FCMP_OEQ}, // Signed
  }, // EQ
  {
    {llvm::CmpInst::Predicate::ICMP_NE, llvm::CmpInst::Predicate::FCMP_UNE}, // Unsigned
    {llvm::CmpInst::Predicate::ICMP_NE, llvm::CmpInst::Predicate::FCMP_UNE}, // Signed
  }, // NE
};

static int get_align(Obj *var) {
  int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;
  return align;
}

static char *get_ident(Token *tok) {
  if (!tok) {
    return "";
  }
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

using llvm::StringRef;

typedef struct BlockScope BlockScope;
struct BlockScope {
  BlockScope *next;

  std::map<std::string, llvm::Value *> vars;
  std::map<std::string, llvm::StructType *> tags;
  std::map<std::string, llvm::Value *> constants;
};

static BlockScope *scope = new BlockScope();

static void enter_scope(void) {
  if (DUMP_SCOPE) {
    output << "enter scope" << std::endl;
  }
  BlockScope *sc = new BlockScope();
  sc->next = scope;
  scope = sc;
}

static void leave_scope(void) {
  if (DUMP_SCOPE) {
    output << "leave scope" << std::endl;
  }
  scope = scope->next;
}

static std::string get_scope_name(Obj *var) {
  std::string var_name = var->name;
  if (var->is_local) {
    int scope_level = var->scope_level;
    var_name.append("..");
    var_name.append(std::to_string(scope_level));
  }
  if (DUMP_SCOPE) {
    output << "get_scope_name:" << var_name << std::endl;
  }
  return var_name;
}

static std::string get_vla_stack_name(Obj *var) {
  std::string var_name = var->name;
  var_name.append(".vla.stack..");
  int scope_level = var->scope_level;
  var_name.append(std::to_string(scope_level));
  return var_name;
}

static std::string get_tag_name(Type *ty) {
  std::string tag_name = get_ident(ty->tag);
  tag_name.append(".");
  tag_name.append(std::to_string(ty->scope_level));
  return tag_name;
}

static llvm::Value *find_var_by_name(std::string var_name) {
  for (BlockScope *sc = scope; sc; sc = sc->next) {
    llvm::Value *v = sc->vars[var_name];
    if (v) {
      return v;
    }
  }
  return nullptr;
}

static llvm::Value *find_var(Obj *var) {
  std::string var_name = get_scope_name(var);
  return find_var_by_name(var_name);
}

static llvm::Value *find_vla_stack(Obj *var) {
  std::string var_name = get_vla_stack_name(var);
  return find_var_by_name(var_name);
}

static void push_basicblock(std::string label, llvm::BasicBlock *BB) {
  sLabelBB[label] = BB;
}

static llvm::BasicBlock *find_basicblock(std::string label) {
  return sLabelBB[label];
}

static void push_var(Obj *var, llvm::Value *v) {
  std::string var_name = get_scope_name(var);
  if (DUMP_SCOPE) {
    output << "push_var:" << var_name << std::endl;
  }
  scope->vars[var_name] = v;
}

static llvm::Value *find_constant(Obj *var) {
  std::string var_name = get_scope_name(var);
  for (BlockScope *sc = scope; sc; sc = sc->next) {
    llvm::Value *v = sc->constants[var_name];
    if (v) {
      return v;
    }
  }
  return nullptr;
}

static void push_constant(Obj *var, llvm::Value *v) {
  std::string var_name = get_scope_name(var);
  scope->constants[var_name] = v;
}

static llvm::StructType *find_tag(std::string tag_name) {
  for (BlockScope *sc = scope; sc; sc = sc->next) {
    llvm::StructType *type = sc->tags[tag_name];
    if (type) {
      return type;
    }
  }
  return nullptr;
}

static llvm::StructType *find_tag(Type *ty) {
  std::string tag_name = get_tag_name(ty);
  return find_tag(tag_name);
}

static void push_tag_scope(std::string tag_name, llvm::StructType *type) {
  scope->tags[tag_name] = type;
}

static void push_tag_scope(Type *ty, llvm::StructType *type) {
  std::string tag_name = get_tag_name(ty);
  push_tag_scope(tag_name, type);
}

void push_packed_tag_scope(Type *ctype, llvm::StructType *type) {
  std::string packed_name = get_struct_name(ctype, true);
  push_tag_scope(packed_name, type);
}

llvm::StructType *find_packed_tag(Type *ctype) {
  std::string packed_name = get_struct_name(ctype, true);
  return find_tag(packed_name);
}

/// Return the value offset.
static llvm::Constant *getOffset(long offset) {
  return llvm::ConstantInt::get(Builder->getInt64Ty(), offset);
}

static llvm::Constant *get_offset_int32(int offset) {
  return llvm::ConstantInt::get(Builder->getInt32Ty(), offset);
}

static void genStore(llvm::Value *V, llvm::Value *Addr);

static llvm::Type *getTypeForArg(Type *ty) {
  llvm::Type *localType;
  if (ty->kind == TY_BOOL) {
    localType = Builder->getInt8Ty();
  } else {
    localType = yuc2LLVMType(ty);
  }
  return localType;
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
  llvm::Function *getIntrinsic(unsigned IID, llvm::ArrayRef<llvm::Type*> Tys = llvm::None);

  llvm::Type *getX86_FP80Ty() {
    return llvm::Type::getX86_FP80Ty(VMContext);
  }
};

static std::unique_ptr<IRGenModule> ModuleBuilder;

static IRGenModule &CGM() {
  return *ModuleBuilder;
}

llvm::Function *IRGenModule::getIntrinsic(unsigned IID,
                                            llvm::ArrayRef<llvm::Type*> Tys) {
  return llvm::Intrinsic::getDeclaration(&getModule(), (llvm::Intrinsic::ID)IID,
                                         Tys);
}


static llvm::Value *retValue = nullptr;
static llvm::Value *allocatedRetValue = nullptr;

static bool isAnnonVar(std::string &name) {
  int index = name.find(".L..");
  return index == 0;
}

static bool is_anony_tag(std::string &name) {
  int index = name.find(".tag..");
  return index == 0; 
}

static std::string get_tag_type_name(Token *tag) {
  std::string tag_name = get_ident(tag);
  return is_anony_tag(tag_name) ? "anon" : tag_name;
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

static void dump_member(Member *member) {
  output<< buildSeperator(stmt_level, "dump_member, idx: ")
        << member->idx  
        << " offset: " << member->offset
        << " is_bitfield: " << member->is_bitfield
        << " bit_offset: " << member->bit_offset
        << " bit_width: " << member->bit_width
        << " ty size: " << member->ty->size
        << " real_type_size: " << member->real_type_size
        << std::endl;
}

static void dump_ctype(Type *ctype) {
  Token *tag = ctype->tag;
  llvm::StringRef suffix = get_tag_type_name(tag).c_str();
  std::string typeStr = ctypeKindString(ctype->kind);
  output << buildSeperator(stmt_level, "dump_ctype, size: ") 
      << ctype->size
      << " kind: " << typeStr
      << " has_bitfield: " << ctype->has_bitfield 
      << " is_typedef:" << ctype->is_typedef
      << " tag: " << suffix.data()
      << " type name: " << get_ident(ctype->name)
      << " align:" << ctype->align << std::endl;
}

static llvm::BasicBlock *createBasicBlock() {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  return llvm::BasicBlock::Create(CGM().getLLVMContext(), "", TheFunction); 
}

static llvm::BasicBlock *getBasicBlock(char *label) {
  llvm::BasicBlock *BB = find_basicblock(label);
  if (!BB) {
    BB = llvm::BasicBlock::Create(CGM().getLLVMContext());
    push_basicblock(label, BB);
  }
  return BB;
}

static llvm::Value* gen_load(llvm::Value *addr) {
  return Builder->CreateLoad(addr->getType()->getNonOpaquePointerElementType(), addr);
}

static llvm::Value* load(Type *ty, llvm::Value *originValue) {
  llvm::Type *type = yuc2LLVMType(ty);
  llvm::Value *V = Builder->CreateLoad(type, originValue);
  if (ty->kind == TY_BOOL) {
    V = Builder->CreateTrunc(V, Builder->getInt1Ty());
  }
  return V;
}

static llvm::Value *gen_rvalue(Node *node, Obj *var) {
  llvm::Value *V = nullptr;
  if (is_const_expr(node)) {
    V = find_constant(var);
    return V;
  }
  llvm::Value *Addr = gen_addr(node);
  Type *varTy = var->ty;
  switch(varTy->kind) {
  case TY_ARRAY:
    V = gen_get_ptr(node, Addr, getOffset(0));
    break;
  case TY_STRUCT:
  case TY_VLA:
    V = Addr;
    break;
  default:
    V = load(varTy, Addr);
    break;
  }
  return V; 
}

static llvm::Value *gen_rvalue(Node *node) {
  int cur_level = ++stmt_level;
  NodeKind kind = node->kind;
  Obj *var = node->var;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(cur_level, "gen_rvalue start:" + kindStr) 
      << " is_const_expr: " << is_const_expr(node) << std::endl;
  llvm::Value *V = gen_rvalue(node, var);
  --stmt_level;
  output << buildSeperator(cur_level, "gen_rvalue end") << std::endl;
  return V;  
}

static llvm::Value *gen_var(Node *node) {
  llvm::Value *Addr = gen_rvalue(node);
  return Addr; 
}

static llvm::Value *gen_subscript_addr(Node *node) {
  llvm::Value *Arr = gen_addr(node->lhs);
  llvm::Value *offset = gen_expr(node->rhs);
  return gen_get_ptr(node->lhs, Arr, offset); 
}

static llvm::Value *gen_subscript(Node *node) {
  llvm::Value *Addr = gen_subscript_addr(node);
  return load(node->ty, Addr); 
}

static Type *get_vla_base_ty(Type *ty) {
  Type *cur = ty;
  while(cur->kind == TY_VLA) {
    cur = cur->base;
  }
  return  cur;
}

static llvm::Value *gen_decl_vla(Node *node) {
  Obj *vla_var = node->lhs->var;
  // load vla length
  Type *ty = vla_var->ty;

  // generate vla ptr
  llvm::Value *L = gen_expr(node->lhs);
  // get vla length for base type
  llvm::Value *vla_size = find_var(ty->vla_size);
  vla_size = load(ty_long, vla_size);
  // get element size
  Type *vla_base_ty = get_vla_base_ty(ty);
  llvm::Value *element_size = llvm::ConstantInt::get(Builder->getInt64Ty(), vla_base_ty->size);
  llvm::Value *vla_len = Builder->CreateSDiv(vla_size, element_size);

  // alloca vla
  llvm::Type *Ty = yuc2LLVMType(vla_base_ty);
  llvm::AllocaInst *localAddr = Builder->CreateAlloca(Ty, vla_len);
  localAddr->setAlignment(llvm::Align(16));
  push_var(vla_var, localAddr);

  return localAddr; 
}

static llvm::Value *gen_vla_ptr(Node *node) {
  llvm::Function *F = CGM().getIntrinsic(llvm::Intrinsic::stacksave);
  llvm::Value *V = Builder->CreateCall(F);
  Obj *var = node->var;
  llvm::Value *Stack = find_vla_stack(var);
  Builder->CreateStore(V, Stack);

  return V; 
}

// check able to cast to primitive type
static bool is_primitive_struct(Type *ctype) {
  if (!ctype->has_bitfield) {
    return false;
  }
  llvm::StructType *type = find_tag(ctype);
  return type->getNumElements() == 1;
}

// cast type for bitfield struct
static llvm::Value *cast_struct_type(llvm::Value *origin, Type *ctype) {
  if (!ctype->has_bitfield) {
    return origin;
  }
  llvm::StructType *targetTy = find_tag(ctype);
  unsigned AS = origin->getType()->getPointerAddressSpace();
  llvm::Type *dest_type = nullptr;
  if (is_primitive_struct(ctype)) {
    // convert to primitive type pointer
    int index = 0;
    dest_type = targetTy->getTypeAtIndex(index)->getPointerTo(AS);
  } else {
    // convert to struct pointer
    dest_type = targetTy->getPointerTo(AS);
  }
  llvm::Value *castedV = Builder->CreateBitCast(origin, dest_type);
  return castedV;
}

static llvm::Value *gen_var_addr(Obj *var) {
  llvm::Value *Addr = nullptr;
  std::string typeStr = ctypeKindString(var->ty->kind);
  std::string varName = var->name;
  output << buildSeperator(stmt_level + 1, "gen_var_addr type:" + typeStr)
    << " is_typedef: " << var->ty->is_typedef
    << " " << varName << std::endl;
  if (var->is_static) {
    Addr = TheModule->getNamedGlobal(varName);
  } if (isAnnonVar(varName)) {
    const std::string &Str = annonInitData[varName];
    Addr = CGM().GetAddrOfConstantCString(Str, nullptr);
  } else if (var->is_local){
    Addr = find_var(var);
  } else {
    Addr = TheModule->getNamedGlobal(varName);
  }
  return Addr;
}

static llvm::Value *gen_addr(Node *node) {
  int cur_level = ++stmt_level;
  llvm::Value *Addr = nullptr;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(cur_level, "gen_addr start:" + kindStr) << std::endl;
  switch(kind) {
    case ND_VAR:
      Addr = gen_var_addr(node->var);
      break;
    case ND_MEMBER:
      Addr = gen_member_addr(node);
      break;
    case ND_DEREF:
      Addr = gen_expr(node->lhs);
      break;
    case ND_SUBSCRIPT: 
      Addr = gen_subscript_addr(node);
      break;
    case ND_COMMA:
      Addr = gen_comma_addr(node);
      break;
    case ND_ASSIGN:
    case ND_COND:
      Addr = gen_expr(node);
      break;
  }
  --stmt_level;
  output << buildSeperator(cur_level, "gen_addr end") << std::endl;
  return Addr;
}

llvm::Value *gen_deref(Node *node) {
  llvm::Value *V = gen_addr(node);
  switch(node->ty->kind) {
  case TY_ARRAY:
    V = gen_get_ptr(node, V, getOffset(0));
    break;
  case TY_STRUCT:
  case TY_VLA:
    break;
  default:
    V = load(node->ty, V);
  }
  return V;
}

static llvm::Value *gen_get_member_ptr(Member *member, llvm::Value *ptrval) { 
  std::vector<llvm::Value *> IndexValues;
  IndexValues.push_back(get_offset_int32(0));
  IndexValues.push_back(get_offset_int32(member->type_idx));
  llvm::Value *target = Builder->CreateInBoundsGEP(ptrval->getType()->getNonOpaquePointerElementType(), ptrval, IndexValues);
  return target;
}

static llvm::Value *gen_member_addr(Node *node) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_member_addr start") << std::endl;
  llvm::Value *base = gen_addr(node->lhs);
  Type *ctype = node->lhs->ty;
  base = cast_struct_type(base, ctype);
  if (is_primitive_struct(ctype)) {
    return base;
  }
  // get elementer ptr for struct member
  llvm::Value *ptr = gen_get_member_ptr(node->member, base);
  --stmt_level;
  output << buildSeperator(cur_level, "gen_member_addr end") << std::endl;
  return ptr;
}

static llvm::Value *gen_cast_bitfield(Member *member, llvm::Value *value) {
  Type *from_type = get_int_type(member->real_type_size);
  Type *to_type = member->ty;
  return cast(value, from_type, to_type);
}

static llvm::Value *gen_trunc_bitfield(Member *member, llvm::Value *value) {
  Type *to_type = get_int_type(member->real_type_size);
  Type *from_type = member->ty;
  return cast(value, from_type, to_type);
}

static llvm::Value *gen_get_bitfield(Member *member, llvm::Value *value) {
  llvm::Value *V = Builder->CreateShl(value, member->lhs_bits);
  V = Builder->CreateAShr(V, member->rhs_bits);
  V = gen_cast_bitfield(member, V);
  return V;
}

static llvm::Value *unify_assign_value(Node *node, llvm::Value *V) {
  if (node->kind != ND_MEMBER) {
    return V;
  }

  Member *member = node->member;
  if (!member->is_bitfield) {
    return V;
  }
  if (!node->ty->is_unsigned) {
    // sigend: trunc digits
    int offset = member->real_type_size * 8 - member->bit_width;
    V = Builder->CreateShl(V, offset);
    V = Builder->CreateAShr(V, offset);
  }

  return gen_cast_bitfield(member, V);
}


static llvm::Value *gen_member(Node *node, llvm::Value **target) {
  output << buildSeperator(stmt_level, "gen_member start") << std::endl;
  llvm::Value *memberAddr = gen_member_addr(node);
  if (target) { 
    *target = memberAddr;
  }
  Member *member = node->member;
  Type *ctype = node->lhs->ty;
  llvm::StructType *type = find_tag(ctype);
  llvm::Type *memberType = type->getTypeAtIndex(member->type_idx);
  llvm::Value *value = Builder->CreateLoad(memberType, memberAddr);
  if (member->is_bitfield) {
    return gen_get_bitfield(member, value);
  }
  return value;
}

static llvm::Type *struct_to_primitive(int size) {
  int target_size = size * 8;
  return llvm::Type::getIntNTy(CGM().getLLVMContext(), target_size);
}

static int get_return_size(Type *struct_ty) {
  output << "struct size:" << struct_ty->size << " align:" << struct_ty->align << std::endl;
  int return_size = struct_ty->size;
  for (Member *member = struct_ty->members; member; member = member->next) {
    output << "member size:" << member->ty->size << " align:" << member->align << " offset: " << member->offset << std::endl;
    if (member->next == nullptr) {
      if (member->offset == 8 && member->ty->size == 1) {
        return_size = 9;
        break;
      }
    }
  }
  return return_size;
}

static llvm::Type *struct_to_return(Type *struct_ty) {
  int size = get_return_size(struct_ty);
  if (size <= 8) {
    return struct_to_primitive(size);
  }
  int left = size - 8;
  if (left <= 8) {
    llvm::SmallVector<llvm::Type*, 2> Types;
    Types.push_back(struct_to_primitive(8));
    Types.push_back(struct_to_primitive(left));
    return llvm::StructType::get(CGM().getLLVMContext(), Types);
  }
  return llvm::Type::getVoidTy(CGM().getLLVMContext());
}

static llvm::Value* cast(llvm::Value *Base, Type *from, Type *to) {
  TypeKind fromKind = from->kind;
  TypeKind toKind = to->kind;
  Type *fromBase = from->base;
  Type *toBase = to->base;

  std::string fromTypeStr = ctypeKindString(fromKind);
  std::string toTypeStr = ctypeKindString(toKind);
  output << buildSeperator(stmt_level, "cast: ")
        << "fromType: " << fromTypeStr << " toType: " << toTypeStr << std::endl;
  if (toKind == TY_VOID
      || (fromKind == TY_VLA && toKind == TY_PTR)) {
    return Base;
  }
  bool sameTypeKind = fromKind == toKind 
                  || (fromKind == TY_ARRAY && toKind == TY_PTR);

  bool sameBaseTypeKind = false;
  if (fromBase && toBase) {
    fromTypeStr = ctypeKindString(fromBase->kind);
    toTypeStr = ctypeKindString(toBase->kind);
    output << buildSeperator(stmt_level, "cast: base")
        << "fromType: " << fromTypeStr << " toType: " << toTypeStr << std::endl;
    sameBaseTypeKind = fromBase->kind == toBase->kind;
  }
   

  if (sameTypeKind && sameBaseTypeKind) {
    output << buildSeperator(stmt_level, "cast: do not cast") << std::endl;
    return Base;
  }

  llvm::Type *targetTy = yuc2LLVMType(to);
  llvm::Value *target = Base;

  if (fromKind == TY_ARRAY && toKind == TY_PTR) {
    return Builder->CreateBitCast(Base, targetTy);
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2]) {
    return cast_table[t1][t2](Base, to);
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

static llvm::Value *CreateConstArrayGEP(Node *node, llvm::Value *baseAddr, uint64_t Index) {
  llvm::Constant *idx = llvm::ConstantInt::get(Builder->getInt64Ty(), Index);

  return gen_get_ptr(node, baseAddr, idx);
}

static llvm::Value *gen_cast(Node *node) {
  int cur_level = stmt_level;
  Node *origin = node->lhs;
  Type *fromType = origin->ty;
  std::string fromTypeStr = ctypeKindString(fromType->kind);
  Type *toType = node->ty;
  std::string toTypeStr = ctypeKindString(toType->kind);
  std::string log = buildSeperator(cur_level, "gen_cast");
  output << log
        << " fromType:" << fromTypeStr
        << " size:" << fromType->size 
        << " unsigned:" << fromType->is_unsigned
        << std::endl;
  output << log
        << " toType:" << toTypeStr 
        << " size:" << toType->size
        << " unsigned:" << toType->is_unsigned
        << std::endl;
  llvm::Value *V = nullptr;
  V = gen_expr(node->lhs);
  V = cast(V, fromType, toType);
  output << buildSeperator(cur_level, "gen_cast end") << std::endl;
  return V;
}

/**
 * char x = 1;
 * char y = &x;
 **/
static llvm::Value *gen_get_addr(llvm::Value *baseAddr) {
  return Builder->CreatePtrToInt(baseAddr, Builder->getInt64Ty());
}

static llvm::Value *gen_stmt_expr(Node *node) {
  llvm::Value *V = nullptr;
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_stmt_expr start") << std::endl;
  enter_scope();
  for (Node *n = node->body; n; n = n->next) {
    gen_stmt(n);
  }
  Node *last = node->last_expr;
  if (last) {
    Obj *lastVar = node->last_var;
    llvm::Value *lastAddr = find_var(lastVar);
    V = load(lastVar->ty, lastAddr);
  }
  leave_scope();
  output << buildSeperator(cur_level, "gen_stmt_expr end") << std::endl;
  --stmt_level;
  return V;
}

static llvm::Value *gen_get_ptr(Node *node, llvm::Value *baseAddr, llvm::Value *offset) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_get_ptr: " + ctypeKindString(node->ty->kind))
       << " baseTy: " << ctypeKindString(node->ty->base->kind) << std::endl; 
  llvm::Value *target = nullptr;
  llvm::Type *origPtrTy = nullptr;
  std::vector<llvm::Value *> IndexValues;
  origPtrTy = baseAddr->getType()->getNonOpaquePointerElementType();
  if (node->ty->kind == TY_ARRAY) {
    IndexValues.push_back(getOffset(0));
  }
  IndexValues.push_back(offset);
  target = Builder->CreateInBoundsGEP(origPtrTy, baseAddr, IndexValues);
  stmt_level--;

  return target;
}

static llvm::Value *gen_add_2(Node *node,
    llvm::Value *operandL, llvm::Value *operandR) {
  llvm::Value *V = nullptr;
  output << buildSeperator(stmt_level, "gen_add_2 o_kind:")
       << node->o_kind << std::endl; 
  
  if (node->o_kind == PTR_NUM || node->o_kind == VLA_NUM) {
    // ptr + num -> GEP(ptr, num)
    // arr + num -> GEP(arr, 0, num)
    V = gen_get_ptr(node->lhs, operandL, operandR);
    return V;
  }

  if (is_flonum(node->ty)) {
    return Builder->CreateFAdd(operandL, operandR);
  }

  if (node->ty->is_unsigned) {
    V = Builder->CreateAdd(operandL, operandR);
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

static llvm::Value *gen_sub(Node *node) {
  llvm::Value *operandL, *operandR, *V;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);
  if (node->o_kind == PTR_PTR) {
    // ptr - ptr
    operandL = gen_get_addr(operandL);
    operandR = gen_get_addr(operandR);
    V = Builder->CreateSub(operandL, operandR);
    if (node->lhs->ty->base) {
      int typeSize = node->lhs->ty->base->size;
      if (typeSize != 1) {
        llvm::Value *SizeV = getOffset(typeSize);
        V = Builder->CreateExactSDiv(V, SizeV);    
      }
    }
    return V;
  } else if (node->o_kind == PTR_NUM) {
    // ptr - num
    operandR = Builder->CreateNeg(operandR);
    V = gen_get_ptr(node, operandL, operandR);
    return V;
  }

  if (is_flonum(node->ty)) {
    return Builder->CreateFSub(operandL, operandR);
  }

  if (node->ty->is_unsigned) {
    V = Builder->CreateSub(operandL, operandR);
  } else {
    V = Builder->CreateNSWSub(operandL, operandR);
  }
  return V;
}

static llvm::Value *gen_mul(Node *node) {
  llvm::Value *operandL, *operandR, *V;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);
  if (is_flonum(node->ty)) {
    return Builder->CreateFMul(operandL, operandR);
  }
  if (node->ty->is_unsigned) {
    V = Builder->CreateMul(operandL, operandR);
  } else {
    V = Builder->CreateNSWMul(operandL, operandR);
  }
  return V;
}

static llvm::Value *gen_div(Node *node) {
  llvm::Value *operandL, *operandR, *V;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);
  if (is_flonum(node->ty)) {
    return Builder->CreateFDiv(operandL, operandR);
  }
  if (node->ty->is_unsigned) {
    V = Builder->CreateUDiv(operandL, operandR);
  } else {
    V = Builder->CreateSDiv(operandL, operandR);
  }
  return V;
}

static llvm::Value *gen_postfix(Node *node, bool isInc) {
  llvm::Value *targetAddr, *operandL, *operandR;

  if (node->lhs->kind == ND_MEMBER) {
    operandL = gen_member(node->lhs, &targetAddr);
  } else {
    targetAddr = gen_addr(node->lhs);
    operandL = load(node->lhs->ty, targetAddr);
  }
  operandR = gen_expr(node->rhs);
  llvm::Value *sum = gen_add_2(node, operandL, operandR);
  
  if (node->lhs->kind == ND_MEMBER) {
    gen_assign_member(targetAddr, sum, node->lhs);
  } else {
    genStore(sum, targetAddr);
  }

  return operandL;
}

static llvm::Value *gen_prefix(Node *node, bool isInc) {
  llvm::Value *targetAddr, *operandL, *operandR;
  if (node->lhs->kind == ND_MEMBER) {
    operandL = gen_member(node->lhs, &targetAddr);
  } else {
    targetAddr = gen_addr(node->lhs);
    operandL = load(node->lhs->ty, targetAddr);
  }
  operandR = gen_expr(node->rhs);
  llvm::Value *sum = gen_add_2(node, operandL, operandR);
  
  if (node->lhs->kind == ND_MEMBER) {
    sum = gen_assign_member(targetAddr, sum, node->lhs);
  } else {
    genStore(sum, targetAddr);
  }
  return sum;
}

static llvm::Value *gen_comma(Node *node) {
  gen_expr(node->lhs);
  llvm::Value *V = gen_expr(node->rhs);
  return V;
}

static llvm::Value *gen_comma_addr(Node *node) {
  gen_expr(node->lhs);
  llvm::Value *V = gen_addr(node->rhs);
  return V;
}

static llvm::Value *gen_cond(Node *node) {
  if (node->ty->kind == TY_VOID) {
    return nullptr;
  }
  llvm::Value *condV, *thenV, *elseV;
  // 0. gen condition
  condV = gen_to_bool(node->cond);
  // both then and els is constant, use select
  if (is_const_expr(node->then) 
    && is_const_expr(node->els)) {
    // fixme
    Builder->CreateZExt(condV, Builder->getInt64Ty());

    thenV = gen_expr(node->then);
    elseV = gen_expr(node->els);
    return Builder->CreateSelect(condV, thenV, elseV);
  }
 
  llvm::BasicBlock *ThenBB = createBasicBlock();
  llvm::BasicBlock *ElseBB = createBasicBlock();
  llvm::BasicBlock *MergeBB = createBasicBlock();

  // 1. create branch
  Builder->CreateCondBr(condV, ThenBB, ElseBB);
  // 2. then expr
  Builder->SetInsertPoint(ThenBB);
  thenV = gen_expr(node->then);
  Builder->CreateBr(MergeBB);
  // 3. else expr
  Builder->SetInsertPoint(ElseBB);
  elseV = gen_expr(node->els);
  Builder->CreateBr(MergeBB);
  // 4. merge basic block
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  Builder->SetInsertPoint(MergeBB);
  // 5. create PHI
  llvm::Type *type = yuc2LLVMType(node->ty);
  llvm::PHINode *phiNode = Builder->CreatePHI(type, 2);
  phiNode->addIncoming(thenV, ThenBB);
  phiNode->addIncoming(elseV, ElseBB);
  return phiNode;
}

static llvm::Value *gen_logic_and(Node *node) {
  llvm::Value *V = nullptr;
   // step1 left operand
  llvm::Value *operandL = gen_to_bool(node->lhs);
  if (auto *constL = dyn_cast<llvm::Constant>(operandL)) {
    // short circuit
    if (constL->isNullValue()) {
      return Builder->getFalse();
    }
    return gen_to_bool(node->rhs);
  }
  llvm::BasicBlock *leftBB = Builder->GetInsertBlock();
  llvm::BasicBlock *rightBB = createBasicBlock();
  llvm::BasicBlock *mergeBB = createBasicBlock();

  Builder->CreateCondBr(operandL, rightBB, mergeBB);
  // step2 right operand
  gen_basicblock(rightBB);
  llvm::Value *operandR = gen_to_bool(node->rhs);
  gen_branch(mergeBB);
  // step3 merge
  gen_basicblock(mergeBB);
  llvm::Type *type = Builder->getInt1Ty();
  llvm::PHINode *phi = Builder->CreatePHI(type, 2);
  phi->addIncoming(Builder->getFalse(), leftBB);
  phi->addIncoming(operandR, rightBB);
  return phi;
}

static llvm::Value *gen_logic_or(Node *node) {
  llvm::Value *V = nullptr;
  // step1 left operand
  llvm::Value *operandL = gen_to_bool(node->lhs);
  if (auto *constL = dyn_cast<llvm::Constant>(operandL)) {
    // short circuit
    if (constL->isOneValue()) {
      return Builder->getTrue();
    }
    return gen_to_bool(node->rhs);
  }
  llvm::BasicBlock *leftBB = Builder->GetInsertBlock();
  llvm::BasicBlock *rightBB = createBasicBlock();
  llvm::BasicBlock *mergeBB = createBasicBlock();

  Builder->CreateCondBr(operandL, mergeBB, rightBB);
  // step2 right operand
  gen_basicblock(rightBB);
  llvm::Value *operandR = gen_to_bool(node->rhs);
  gen_branch(mergeBB);
  // step3 merge
  gen_basicblock(mergeBB);
  llvm::Type *type = Builder->getInt1Ty();
  llvm::PHINode *phi = Builder->CreatePHI(type, 2);
  phi->addIncoming(Builder->getTrue(), leftBB);
  phi->addIncoming(operandR, rightBB);
  return phi;
}

static llvm::Value *gen_not(Node *node) {
  llvm::Value *operand;
  operand = gen_to_bool(node->lhs);
  operand = Builder->CreateNot(operand);
  if (node->eval_as_bool) {
    return operand;
  }
  return Builder->CreateZExt(operand, Builder->getInt32Ty());
}

static llvm::Value *gen_bitnot(Node *node) {
  llvm::Value *operand;
  operand = gen_expr(node->lhs);
  return Builder->CreateXor(operand, -1);
}

static llvm::Value *emit_memcpy(llvm::Value *dest, llvm::Value *src, Type *ty) {
  llvm::Value *Size = getOffset(ty->size);
  llvm::MaybeAlign Align = llvm::MaybeAlign(ty->align);
  return Builder->CreateMemCpy(dest, Align, src, Align, Size);
}

static llvm::Value *emit_struct_condition(Node *node, llvm::Value *dest) {
  output << buildSeperator(stmt_level, "emit_struct_condition") << std::endl;
  llvm::Value *condV, *thenV, *elseV;
  // 0. gen condition
  condV = gen_to_bool(node->cond);

  llvm::BasicBlock *ThenBB = createBasicBlock();
  llvm::BasicBlock *ElseBB = createBasicBlock();
  llvm::BasicBlock *MergeBB = createBasicBlock();
  // 1. create branch
  Builder->CreateCondBr(condV, ThenBB, ElseBB);
  // 2. then expr
  Builder->SetInsertPoint(ThenBB);
  llvm::Value *target_addr = uniform_address(dest);
  thenV = gen_expr(node->then);
  llvm::Value *src_addr = uniform_address(thenV);
  emit_memcpy(target_addr, src_addr, node->then->ty);
  Builder->CreateBr(MergeBB);
  // 3. else expr
  Builder->SetInsertPoint(ElseBB);
  target_addr = uniform_address(dest);
  elseV = gen_expr(node->els);
  src_addr = uniform_address(elseV);
  emit_memcpy(target_addr, src_addr, node->els->ty);  
  Builder->CreateBr(MergeBB);
  // 4. merge basic block
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  Builder->SetInsertPoint(MergeBB);
  return dest;
}

static llvm::Value *emit_assign_struct(Node *node) {
  output << buildSeperator(stmt_level, "emit_assign_struct") << std::endl;
  llvm::Value *addrL, *addrR, *operandL, *operandR;
  addrL = gen_addr(node->lhs);
  if (node->rhs->kind == ND_COND) {
    // case: struct T x, y, z; int a =3;
    // z = a ? x : y;
    emit_struct_condition(node->rhs, addrL);
    return addrL;
  }
  if (node->lhs->kind == ND_DEREF) {
    addrR = gen_addr(node->rhs);
    operandL = uniform_address(addrL);
  } else {
    operandL = uniform_address(addrL);
    addrR = gen_addr(node->rhs);
  }
  operandR = uniform_address(addrR);
  
  llvm::Value *dest = emit_memcpy(operandL, operandR, node->ty);
  return addrR;
}


static llvm::Constant *gen_struct_literal(Obj *var) {
  Type *var_type = var->ty;
  char *buf = var->init_data;
  llvm::StructType *type = get_init_struct_type(var_type);
  return init_struct(type, var_type, buf);
}

static llvm::Constant *gen_literal(Node *node) {
  Obj *var = node->var;
  if (!var) {
    return static_cast<llvm::Constant *>(gen_expr(node));
  }

  Type *var_type = var->ty;
  output << buildSeperator(stmt_level, "gen_literal start:") 
      << " is_struct: " << is_struct(var->ty)
      << std::endl;

  if (is_struct(var_type)) {
    return gen_struct_literal(var);
  }
  return static_cast<llvm::Constant *>(gen_expr(var->init->expr));
}

static llvm::GlobalVariable *gen_const_variable(Obj *var, llvm::Constant *initializer) {
  std::string var_name = var->name;
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  std::string function_name =  TheFunction->getName().data();
  std::string name = "__const." + function_name + "." +var_name;
  Type *ctype = var->ty;

  unsigned AddrSpace = 0;
  llvm::Module &M = CGM().getModule();
  // Create a global variable for this struct
  llvm::GlobalValue::LinkageTypes linkage_type = llvm::GlobalValue::PrivateLinkage;
  auto *gVar = new llvm::GlobalVariable(
      M, 
      initializer->getType(), 
      true,
      linkage_type, 
      initializer, 
      name,
      nullptr, 
      llvm::GlobalVariable::NotThreadLocal, 
      AddrSpace);

  gVar->setAlignment(llvm::MaybeAlign(ctype->align));
  gVar->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return gVar;
}

static llvm::Value *gen_memzero(Node *node) {
  llvm::Constant *operandR = nullptr;
  Obj *var = node->var;
  if (!var) {
    return operandR;
  }
  bool is_struct_ty = is_struct(var->ty);
  bool is_const_value = is_const_initializer(var->init);
  output << buildSeperator(stmt_level, "gen_memzero start:") 
    << " is_struct: " << is_struct_ty
    << " is_const_init: " << is_const_value
    << std::endl;
  if (!is_const_value || !is_struct_ty) {
    return operandR;
  }
  operandR = gen_literal(node);
  llvm::GlobalVariable * global_var = gen_const_variable(var, operandR);

  llvm::Value *operandL = gen_var_addr(var);
  operandL = uniform_address(operandL);
  llvm::Value *src = uniform_address(global_var);
  llvm::Value *dest = emit_memcpy(operandL, src, var->ty);

  return dest;
}

static llvm::Value *gen_assign_member(llvm::Value *member_addr, llvm::Value *rhs, Node *lhs) {
  Member *member = lhs->member;
  if (!member->is_bitfield) {
    // store masked value to bit_field member
    genStore(rhs, member_addr);
    return rhs;
  }
  // trunc rhs
  llvm::Value *R = gen_trunc_bitfield(member, rhs);
  // load lhs
  llvm::Value *member_value = gen_load(member_addr);
  // mask rhs
  long rhs_mask = (1L << member->bit_width) - 1; 
  llvm::Value *and_R = Builder->CreateAnd(R, rhs_mask);
  
  // get masked bitfield value
  int bit_offset = member->real_type_size * 8 - member->bit_width - member->lhs_bits;
  // shift left rhs
  if (bit_offset > 0) {
    R = Builder->CreateShl(and_R, bit_offset);
  } else {
    R = and_R;
  }

  // mask lhs
  long mask = ((1L << member->bit_width) - 1) << bit_offset;
  llvm::Value *masked_value = Builder->CreateAnd(member_value, ~mask);
  
  // get final bitfield value
  masked_value = Builder->CreateOr(masked_value, R);

  // store masked value to bit_field member
  genStore(masked_value, member_addr);

  return unify_assign_value(lhs, and_R);
}

static llvm::Value *gen_assign(Node *node) {
  Node *lhs = node->lhs;
  output << buildSeperator(stmt_level, "gen_assign start:") 
      << " is_const_expr: " << is_const_expr(lhs)
      << " is_struct: " << is_struct(node->ty)
      << std::endl;
  if (is_struct(node->ty)) {
   return emit_assign_struct(node);
  }
  llvm::Value *operandL, *operandR;
  operandR = gen_expr(node->rhs);
  operandL = gen_addr(node->lhs);
  if (node->lhs->kind == ND_MEMBER) {
    return gen_assign_member(operandL, operandR, node->lhs);
  } 
  genStore(operandR, operandL);
  if (is_const_expr(lhs)) {
     push_constant(lhs->var, operandR);
  }
  return operandR;
}

static bool isNoReturn(std::string name) {
  bool BuildSinks
    = llvm::StringSwitch<bool>(StringRef(name))
      .Case("exit", true)
      .Case("panic", true)
      .Case("error", true)
      .Case("Assert", true)
      .Default(false);
  return BuildSinks;
}

static llvm::Value *builtin_alloca(Node *node) {
  llvm::Value *arg = gen_expr(node->args);
  llvm::Type *ty = Builder->getInt8Ty();
  llvm::AllocaInst * localAddr = Builder->CreateAlloca(ty, arg);
  localAddr->setAlignment(llvm::Align(16));
  return localAddr;
} 

static llvm::Value *builtin_memcpy(Node *node) {
  Node *dst = node->args;
  Node *src = dst->next;
  Node *size = src->next;
  llvm::Value *Dst = gen_expr(dst);
  llvm::Value *Src = gen_expr(src);
  llvm::Value *Size = gen_expr(size);
  llvm::MaybeAlign Align = llvm::MaybeAlign(1);
  llvm::CallInst *callInst 
    = Builder->CreateMemCpy(Dst, Align, Src, Align, Size);
  return callInst;
}

static llvm::Value *emit_call(Node *node) {
  std::string Callee = node->lhs->var->name;
  output <<"emit_call: " << Callee << std::endl;
  if (Callee == "alloca") {
    return builtin_alloca(node);
  } else if (Callee == "memcpy") {
    return builtin_memcpy(node);
  }
  std::vector<llvm::Value *> ArgsV = push_args(node);
  llvm::Function *CalleeF = getFunction(Callee);
  llvm::Value *V = Builder->CreateCall(CalleeF, ArgsV, "");

  if (isNoReturn(Callee)) {
    Builder->CreateUnreachable();
    Builder->ClearInsertionPoint();
  }
  return V;
}

static llvm::CmpInst::Predicate getPredicate(Node *node, Type *nodeType) {
  llvm::CmpInst::Predicate predicate;
  int relationId = getRelationId(node);
  int signId = getSignId(nodeType);
  int catagory = getTypeCatagory(nodeType);
  predicate = predicate_table[relationId][signId][catagory];
  return predicate;
}

static llvm::Value *gen_relational(Node *node) {
  output <<"gen_relational " << std::endl;
  llvm::Value *operandL, *operandR, *V;
  operandL = gen_expr(node->lhs);
  operandR = gen_expr(node->rhs);
  llvm::CmpInst::Predicate predicate = getPredicate(node, node->lhs->ty);
  V = Builder->CreateCmp(predicate, operandL, operandR);
  if (node->eval_as_bool) {
    return V;
  }
  return cast(V, ty_bool, ty_int);
}

static llvm::Value *gen_mod(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  llvm::Value *V = nullptr;
  if (node->ty->is_unsigned) {
    V = Builder->CreateURem(operandL, operandR);
  } else {
    V = Builder->CreateSRem(operandL, operandR);
  }
  return V;
}

static llvm::Value *gen_bitand(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  return Builder->CreateAnd(operandL, operandR);
}

static llvm::Value *gen_bitor(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  return Builder->CreateOr(operandL, operandR);
}

static llvm::Value *gen_bitxor(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  return Builder->CreateXor(operandL, operandR);
}

static llvm::Value *gen_shl(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  llvm::Value *V = Builder->CreateShl(operandL, operandR);
  return V;
}

static llvm::Value *gen_shr(Node *node) {
  llvm::Value *operandL = gen_expr(node->lhs);
  llvm::Value *operandR = gen_expr(node->rhs);
  llvm::Value *V = nullptr;
  if (node->ty->is_unsigned) {
    V = Builder->CreateLShr(operandL, operandR);
  } else {
    V = Builder->CreateAShr(operandL, operandR);
  }
  return V;
}

static llvm::Constant *gen_number_for_type(llvm::Type *type, Node *node) {
  Type *nodeType = node->ty;
  if (is_integer(nodeType)) {
    uint64_t val = node->val;
    output << buildSeperator(stmt_level, "gen_number int:") << val << std::endl; 
    return llvm::ConstantInt::get(type, val);
  }
  long double fval = node->fval;
  output << buildSeperator(stmt_level, "gen_number double:") << fval << std::endl; 
  return llvm::ConstantFP::get(type, fval);
}

static llvm::Value *gen_number(Node *node) {
  Type *nodeType = node->ty;
  return gen_number_for_type(yuc2LLVMType(nodeType), node);
}

static llvm::Value *gen_neg(Node *node) {
  Node *target = node->lhs->lhs;
  llvm::Value *targetV = gen_expr(target);
  if (is_flonum(target->ty)) {
    return Builder->CreateFNeg(targetV);
  }
  return Builder->CreateNeg(targetV);
}

static llvm::Value *gen_expr(Node *node) {
  int cur_level = stmt_level;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  Type* nodeType = node->ty;
  std::string typeStr = nodeType ? ctypeKindString(nodeType->kind) : "unknow type";
  output << buildSeperator(cur_level, "gen_expr start, kind:" + kindStr + " type:" + typeStr) << std::endl; 
  llvm::Value *V = nullptr;
  cur_level++;
  llvm::Value *casted = nullptr;
  llvm::Value *operandL, *operandR;
  ++stmt_level;
  switch(kind) {
    case ND_NULL_EXPR:
      break;
    case ND_COMMA:
      V = gen_comma(node);
      break;
    case ND_COND:
      V = gen_cond(node);
      break;
    case ND_NOT:
      V = gen_not(node);
      break;
    case ND_BITNOT:
      V = gen_bitnot(node);
      break;
    case ND_LOGAND:
      V = gen_logic_and(node);
      break;
    case ND_LOGOR:
      V = gen_logic_or(node);
      break; 
    case ND_ASSIGN:
      V = gen_assign(node);
      break;
    case ND_NUM:
      V = gen_number(node);
      break;
    case ND_NEG:
      V = gen_neg(node);
      break;
    case ND_MEMZERO:
      V = gen_memzero(node);
      break;
    case ND_VAR:
      V = gen_var(node);
      break;
    case ND_MEMBER:
      V = gen_member(node, nullptr);
      break;
    case ND_DEREF:
      V = gen_deref(node);
      break;
    case ND_ADDR:
      V = gen_addr(node->lhs);
      break;
    case ND_STMT_EXPR:
      V = gen_stmt_expr(node);
      break;
    case ND_CAST:
      V = gen_cast(node);
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
      V = gen_sub(node);
      break;
    case ND_MUL:
      V = gen_mul(node);
      break;
    case ND_DIV:
      V = gen_div(node);
      break;
    case ND_MOD:
      V = gen_mod(node);
      break;
    case ND_BITAND:
      V = gen_bitand(node);
      break;
    case ND_BITOR:
      V = gen_bitor(node);
      break;
    case ND_BITXOR:
      V = gen_bitxor(node);
      break;
    case ND_FUNCALL:
      V = emit_call(node);
      break;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      V = gen_relational(node);
      break;
    case ND_SHL:
      V = gen_shl(node);
      break;
    case ND_SHR:
      V = gen_shr(node);
      break;
    case ND_SUBSCRIPT:
      V = gen_subscript(node);
      break;
    case ND_DECL_VLA:
      V = gen_decl_vla(node);
      break;
    case ND_VLA_PTR:
      V = gen_vla_ptr(node);
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
  llvm::Value *condValue = gen_to_bool(node->cond);
  // constant propogate
  if (auto *constant = dyn_cast<llvm::Constant>(condValue)) {
    if (constant->isZeroValue()) {
      if (node->els) 
        gen_stmt(node->els);
    } else {
      gen_stmt(node->then);
    }
    return;
  }

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(CGM().getLLVMContext(), "", TheFunction);
  llvm::BasicBlock *ElseBB = node->els ? llvm::BasicBlock::Create(CGM().getLLVMContext()) : nullptr;
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(CGM().getLLVMContext());

  Builder->CreateCondBr(condValue, ThenBB, ElseBB ? ElseBB : MergeBB);
  
  Builder->SetInsertPoint(ThenBB);
  gen_stmt(node->then);
  gen_branch(MergeBB);
  if (ElseBB) {
    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder->SetInsertPoint(ElseBB);
    gen_stmt(node->els);
    gen_branch(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    // ElseBB = Builder->GetInsertBlock();
  }

  // Emit merge block.
  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder->SetInsertPoint(MergeBB);
}

static void gen_do(Node *node) {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(CGM().getLLVMContext());
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(CGM().getLLVMContext());
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(CGM().getLLVMContext());

  // label for break and continue
  push_basicblock(node->brk_label, MergeBB);
  push_basicblock(node->cont_label, CondBB);

  // step 1 gen body
  gen_branch(ThenBB);
  TheFunction->getBasicBlockList().push_back(ThenBB);
  Builder->SetInsertPoint(ThenBB);
  gen_stmt(node->then);

  // step 2 jump to cond
  gen_branch(CondBB);
  TheFunction->getBasicBlockList().push_back(CondBB);
  Builder->SetInsertPoint(CondBB);
  llvm::Value *condValue = gen_to_bool(node->cond);
  Builder->CreateCondBr(condValue, ThenBB, MergeBB);

  // Emit merge block.
  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder->SetInsertPoint(MergeBB);
}

static void add_switch_case(Node *node, llvm::SwitchInst *switchInst) {
  if (node) {
    add_switch_case(node->case_next, switchInst);
    int caseV = node->begin;
    while(caseV <= node->end) {
      llvm::BasicBlock *BB = getBasicBlock(node->label);
      switchInst->addCase(Builder->getInt32(caseV), BB);
      caseV++;
    }
    // [GNU] Case ranges
  }
}

static void gen_switch(Node *node) {
  // gen swith condition
  llvm::Value *condV = gen_expr(node->cond);
  if (auto *constV = dyn_cast<llvm::ConstantInt>(condV)) {
    int condInt = constV->getSExtValue();
    // fixme constant fold
    output << "condInt: " << condInt << std::endl;
  }
  char *label = node->default_case ? 
    node->default_case->label : node->brk_label;
  // gen switch branch
  llvm::BasicBlock *defaultBB= getBasicBlock(label);
  llvm::SwitchInst *switchInst = Builder->CreateSwitch(condV, defaultBB);
  // collect case
  add_switch_case(node->case_next, switchInst);
  // gen switch body
  gen_stmt(node->then);
  llvm::BasicBlock *breakBB = getBasicBlock(node->brk_label);
  // gen branch for default case
  gen_branch(breakBB);
  // out of switch
  gen_basicblock(breakBB); 
}
static void gen_for(Node *node) {
  // step 1 gen init
  if (node->init)
    gen_stmt(node->init);

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *CondBB = node->cond ? llvm::BasicBlock::Create(CGM().getLLVMContext()) : nullptr;
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(CGM().getLLVMContext());
  llvm::BasicBlock *IncBB = node->inc ? llvm::BasicBlock::Create(CGM().getLLVMContext()) : nullptr;
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(CGM().getLLVMContext());

  push_basicblock(node->brk_label, MergeBB);
  push_basicblock(node->cont_label, IncBB ? IncBB : (CondBB ? CondBB : ThenBB));
  // step 2 jump to CondBB
  if (CondBB) {    
    Builder->CreateBr(CondBB);
    TheFunction->getBasicBlockList().push_back(CondBB);
    Builder->SetInsertPoint(CondBB);
    llvm::Value *condValue = gen_to_bool(node->cond);
    Builder->CreateCondBr(condValue, ThenBB, MergeBB);
  } else {
    Builder->CreateBr(ThenBB);
  }
  
  // step 3 gen loop body
  TheFunction->getBasicBlockList().push_back(ThenBB);
  Builder->SetInsertPoint(ThenBB);
  gen_stmt(node->then);
  //ThenBB = Builder->GetInsertBlock();

  // step 4 jump to inc
  if (IncBB) {
    gen_branch(IncBB);
    TheFunction->getBasicBlockList().push_back(IncBB);
    Builder->SetInsertPoint(IncBB);
    gen_expr(node->inc);
    //IncBB = Builder->GetInsertBlock();
  }

  llvm::BasicBlock *curBB = Builder->GetInsertBlock();
  // step 5 jump back to cond or body
  if (CondBB) {
    gen_branch(CondBB);
  } else {
    gen_branch(ThenBB);
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

void findAndReplaceAll(std::string & data, std::string toSearch, std::string replaceStr)
{
    // Get the first occurrence
    size_t pos = data.find(toSearch);
    // Repeat till end is reached
    while( pos != std::string::npos)
    {
        // Replace this occurrence of Sub String
        data.replace(pos, toSearch.size(), replaceStr);
        // Get the next occurrence from the current position
        pos =data.find(toSearch, pos + replaceStr.size());
    }
}

static llvm::Value *gen_asm(Node *node) {
  // Assemble the final asm string.
  std::string AsmString = node->asm_str;
  findAndReplaceAll(AsmString, "$", "$$");

  std::string Constraints = "~{dirflag},~{fpsr},~{flags}";
  bool HasUnwindClobber = false;
  std::vector<llvm::Type *> ArgTypes;
  llvm::Type *ResultType = Builder->getVoidTy();
  llvm::FunctionType *FTy =
    llvm::FunctionType::get(ResultType, ArgTypes, false);
  bool HasSideEffect = true;
  llvm::InlineAsm::AsmDialect AsmDialect = llvm::InlineAsm::AD_ATT;
  llvm::InlineAsm *IA = llvm::InlineAsm::get(
      FTy, AsmString, Constraints, HasSideEffect,
      /* IsAlignStack */ false, AsmDialect, HasUnwindClobber);

  std::vector<llvm::Value*> RegResults;
  llvm::Value *V = Builder->CreateCall(IA, RegResults);

  return nullptr;
}

static void gen_block_item(Node *node) {
  bool is_goto = false;
  for (Node *n = node->body; n; n = n->next) {
    // last stmt is goto, current is not label, ingore next stmt
    if (is_goto && n->kind != ND_LABEL && n->kind != ND_CASE) {
       break;
    }
    gen_stmt(n);
    is_goto = n->kind == ND_GOTO;
  }
}

static void gen_block(Node *node) {
  enter_scope();
  gen_block_item(node);
  leave_scope();
}

static void gen_goto(Node *node) {
  llvm::BasicBlock *BB = getBasicBlock(node->unique_label);
  Builder->CreateBr(BB);
}

static void gen_branch(llvm::BasicBlock *target) {
  llvm::BasicBlock *src = Builder->GetInsertBlock();
  if (src == nullptr || target == nullptr) {
    return;
  }
  llvm::Instruction* terminator = src->getTerminator();
  if (!terminator) {
    Builder->CreateBr(target);
  }
}

static void gen_basicblock(llvm::BasicBlock *targetBB) {
  llvm::BasicBlock *curBB = Builder->GetInsertBlock();
  llvm::Function *TheFunction = curBB->getParent();
  if (curBB != targetBB) {
    TheFunction->getBasicBlockList().push_back(targetBB);
    Builder->SetInsertPoint(targetBB);
  }
}

static void gen_label(Node *node) {
  llvm::BasicBlock *BB = getBasicBlock(node->unique_label);
  gen_branch(BB);
  gen_basicblock(BB);
  gen_stmt(node->lhs);
}

static void gen_case(Node *node) {
  llvm::BasicBlock *BB = getBasicBlock(node->label);
  gen_branch(BB);
  gen_basicblock(BB);
  gen_stmt(node->lhs);
}

static void gen_stmt(Node *node) {
  if (!node) {
    return;
  }
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  NodeKind kind = node->kind;
  std::string kindStr = node_kind_info(kind);
  output << buildSeperator(level, "gen_stmt start:")
        << " "<< cur_count << " " << level
        << " " << kindStr << std::endl;
  llvm::Value *tmpV;
  switch(kind) {
    case ND_IF:
      gen_if(node);
      break;
    case ND_FOR:
      gen_for(node);
      break;
    case ND_DO:
      gen_do(node);
      break;
    case ND_SWITCH:
      gen_switch(node);
      break;
    case ND_CASE:
      gen_case(node);
      break;
    case ND_BLOCK:
      gen_block(node);
      break;
    case ND_GOTO:
    case ND_GOTO_EXPR:
      gen_goto(node);
      break;
    case ND_LABEL:
      gen_label(node);
      break;
    case ND_BLOCK_ITEM:
      gen_block_item(node);
      break;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      break;
    case ND_RETURN:
      gen_return(node);
      break;
    case ND_ASM:
      gen_asm(node);
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

static llvm::Type *emit_pointer_type(Type *ctype) {
  Type *baseTy = ctype->base;
  llvm::Type *base = nullptr;
  if (baseTy->kind == TY_VOID) {
    base = Builder->getInt8Ty();
  } else {
    base = yuc2LLVMType(baseTy);
  }
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

static llvm::Type* get_padding_type(int align, int size) {
  int padSize = align - size;
  return padSize > 0 ? getPaddingType(padSize) : nullptr;
}

static bool is_aligned(int offset, int size) {
  return offset % size == 0;
}

static llvm::StructType *create_struct_type(Type *ctype) {
  // struct
  llvm::StructType *type = llvm::StructType::create(*TheContext);
  addRecordTypeName(ctype, type);
  push_tag_scope(ctype, type);
  return type;
}

static void compute_bit_shift(Member *start, Member *end, int type_size, int start_bit_offset) {
  Member *member = start;
  int total_bits = type_size * 8;
  while(member != end) {
    int rhs_bits = total_bits - member->bit_width;
    int lhs_bits = rhs_bits - (member->bit_offset - start_bit_offset);
    member->rhs_bits = rhs_bits;
    member->lhs_bits = lhs_bits;
    member->real_type_size = type_size;
    member = member->next;
  }
}

static llvm::StructType *yuc2StructType(Type *ctype) {
  std::vector<llvm::Type *> Types;

  Member *member = ctype->union_field;
  llvm::StructType *type;
  int DesiredSize = ctype->size;

  dump_ctype(ctype);

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
  bool packed = false;

  int index = 0;
  member = ctype->members;
  while (member) {
    dump_member(member);
    if (member->is_bitfield) {
      Member *mem_start = member, *mem_end;
      int bit_width = member->bit_width;
      int bit_offset = member->bit_offset;
      int max_align = member->align;
      int offset = member->offset;

      member->type_idx = index;
      member = member->next;
      while(member && member->is_bitfield && member->offset == offset ) {
        dump_member(member);
        bit_width += member->bit_width;
        member->type_idx = index;
        if (max_align > member->align) {
          max_align = member->align;
        }
        member = member->next;
      }
      mem_end = member;
      int type_size = align_to_eight(bit_width);
      output << "after grouped, offset: " << offset 
        << " bit_width: " << bit_width 
        << " type_size: " << type_size
        << " max_align: " << max_align
        << std::endl;
      compute_bit_shift(mem_start, mem_end, type_size, bit_offset);
      Types.push_back(get_int_n_ty(type_size));
      index++;

      if (!is_aligned(bit_offset / 8, type_size)) {
        packed = true;
      }

      int byte_size = type_size + bit_offset / 8;
      llvm::Type *paddingType = get_padding_type(max_align, byte_size);
      if (paddingType) {
        Types.push_back(paddingType);
        index++;
      }
    } else {
      Types.push_back(yuc2LLVMType(member->ty));
      member->type_idx = index++;
      llvm::Type *paddingType = get_padding_type(member->align, member->ty->size);
      if (paddingType) {
        Types.push_back(paddingType);
        index++;
      }
      member = member->next;
    }
  }
  type = create_struct_type(ctype);
  type->setBody(Types, packed);
  if (ctype->has_bitfield) {
    create_packed_struct_type(type, ctype);
  }
  return type;
}


std::string get_struct_name(Type *ctype, bool packed) {
  llvm::SmallString<256> TypeName;
  llvm::raw_svector_ostream OS(TypeName);
  std::string kindName = ctypeKindString(ctype->kind);
  OS << kindName << ".";
  // tag
  std::string tag_name = get_tag_type_name(ctype->tag);
  OS << tag_name;
  if (packed) {
    OS << ".packed";
  }
  return OS.str().str();
}

static void addRecordTypeName(Type *ctype, llvm::StructType *Ty) {
  Ty->setName(get_struct_name(ctype, false));
  output << "addRecordTypeName: " << Ty->getName().data() << std::endl;
}


static llvm::StructType *ConvertRecordDeclType(Type *ctype) {
  llvm::StructType *ty = find_tag(ctype);
  if (ty) {
    return ty;
  }
  llvm::StructType *DesiredType = yuc2StructType(ctype);
  return DesiredType;
}

static llvm::Type *yuc2LLVMType(Type *ctype) {
  //output << "yuc2LLVMType: " << ctypeKindString(ctype->kind)  << std::endl;
  llvm::Type *type;
  switch (ctype->kind) {
    case TY_BOOL:
      type = Builder->getInt8Ty();
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
    case TY_FLOAT:
      type = Builder->getFloatTy();
      break;
    case TY_DOUBLE:
      type = Builder->getDoubleTy();
      break;
    case TY_LDOUBLE:
      type = CGM().getX86_FP80Ty();
      break;
    case TY_PTR:
     	type = emit_pointer_type(ctype);
      break;
    case TY_ARRAY:
      type = yuc2ArrayType(ctype);
      break;
    case TY_UNION:
    case TY_STRUCT:
      type = ConvertRecordDeclType(ctype);
      break;
    case TY_VOID:
      type = Builder->getVoidTy();
      break;
    default:
      type = Builder->getInt32Ty();
      break;
  }
  return type;
}

static llvm::Type *get_init_type(Type *ctype) {
  llvm::Type *normal_type = yuc2LLVMType(ctype);
  if (ctype->has_bitfield) {
    return find_packed_tag(ctype);
  }
  return normal_type;
}

static llvm::StructType *get_init_struct_type(Type *ctype) {
  if (ctype->has_bitfield) {
    return find_packed_tag(ctype);
  }
  return find_tag(ctype);
}

llvm::Constant *create_initializer(llvm::Type *origin_type, Obj *var) {
  Type *ctype = var->ty;
  llvm::Type *normal_type = ctype->has_bitfield ? find_tag(ctype) : origin_type;
  return yuc2Constants(normal_type, var);
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


int get_grouped_bitfield_size(Member **rest, Member *member, int offset) {
  int bit_width = 0;
  while(member && member->is_bitfield && member->offset == offset) {
    dump_member(member);
    bit_width += member->bit_width;
    member = member->next;
  }
  *rest = member;
  return align_to_eight(bit_width);
}


llvm::Constant *init_bitfield_struct(llvm::StructType *type, Type *ctype, char *buf) {
  std::vector<llvm::Constant *> elements;

  Member *member = ctype->members;
  int pre_index = -1;
  int offset = 0;
  while(member) {
    Member *end;
    int index = member->type_idx;
    if (pre_index + 1 < index) {
      llvm::Type *memberTy = type->getTypeAtIndex(pre_index + 1);
      llvm::Constant *v = llvm::UndefValue::get(memberTy);
      elements.push_back(v);
    }

    if (!member->is_bitfield) {
      llvm::Type *memberTy = type->getTypeAtIndex(index);
      llvm::Constant *v = buffer2Constants(memberTy, member->ty, NULL, buf, member->offset);
      elements.push_back(v);
      end = member->next;
      offset = member->offset + member->ty->size;
    } else {
      size_t grouped_size = get_grouped_bitfield_size(&end, member, member->offset);
      llvm::Type *i8ty = Builder->getInt8Ty();
      int start_index = member->offset + member->bit_offset / 8;
      offset = start_index + grouped_size;
      char *start = buf + start_index;
      for (int i = 0; i < grouped_size; i++) {
        llvm::Constant *v = llvm::ConstantInt::get(i8ty, read_buf(start + i, 1));
        elements.push_back(v);
      }
    }
    pre_index = index;
    member = end;
  }

  int delta = ctype->size - offset;
  if (delta > 0) {
    llvm::Type *memberTy = get_int_n_ty(delta);
    llvm::Constant *v = llvm::UndefValue::get(memberTy);
    elements.push_back(v);
  }
  llvm::StructType *packed_type = find_packed_tag(ctype);
  return llvm::ConstantStruct::get(packed_type, elements);
}

static llvm::StructType *create_packed_struct_type(llvm::StructType *normal_type, Type *ctype) {
  std::vector<llvm::Type *> types;

  Member *member = ctype->members;
  int pre_index = -1;
  int offset = 0;
  while(member) {
    Member *end;
    int index = member->type_idx;
    
    if (pre_index + 1 < member->type_idx) {
      llvm::Type *memberTy = normal_type->getTypeAtIndex(pre_index + 1);
      types.push_back(memberTy);
    }

    if (!member->is_bitfield) {
      llvm::Type *memberTy = normal_type->getTypeAtIndex(index);
      types.push_back(memberTy);
      end = member->next;
      offset = member->offset + member->ty->size;
    } else {
      size_t grouped_size = get_grouped_bitfield_size(&end, member, member->offset);
      int start_index = member->offset + member->bit_offset / 8;
      offset = start_index + grouped_size;
      llvm::Type *i8ty = Builder->getInt8Ty();
      for (int i = 0; i < grouped_size; i++) {
        types.push_back(i8ty);
      }
    }
    pre_index = index;
    member = end;
  }
  output << "offset: " << offset << " struct size: " << ctype->size << std::endl;
  llvm::Type *tail_padding = get_padding_type(ctype->size, offset);
  if (tail_padding) {
    types.push_back(tail_padding);
  }
  llvm::StructType *type = llvm::StructType::get(*TheContext, types);
  push_packed_tag_scope(ctype, type);
  return type;
}

llvm::Constant *EmitRecordInitialization(llvm::StructType *Ty, Type *ctype, char *buf, int offset) {
  std::vector<llvm::Constant *> Elements;
  Member *member = ctype->members;
  while(member) {
    // Type *Ty = yuc2LLVMType(member->ty);
    int index = member->type_idx;
    llvm::Type *memberTy = Ty->getTypeAtIndex(index);
    llvm::Constant *constantValue = buffer2Constants(memberTy, member->ty, NULL, buf, offset + member->offset);
    Elements.push_back(constantValue);
    member = member->next;
    while(member && member->type_idx == index) {
      member = member->next;
    }
  }

  return llvm::ConstantStruct::get(Ty, Elements);
}

static llvm::Constant *init_struct(llvm::StructType *type, Type *ctype, char *buf) {
  if (ctype->has_bitfield) {
    return init_bitfield_struct(type, ctype, buf);
  }
  return EmitRecordInitialization(type, ctype, buf, 0);
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
    return llvm::Constant::getNullValue(destTy);
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

llvm::Constant *EmitFloatInitialication(llvm::Type *destTy, Type *IntType, char *buf, int offset, Relocation *rel) {
  int size = IntType->size;
  if (!buf) {
    return llvm::Constant::getNullValue(destTy);
  }
  if (!rel) {
    float fVal = *(float *)(buf+offset);
    return llvm::ConstantFP::get(CGM().getLLVMContext(), llvm::APFloat(fVal));
  }
  output << "label: " << *rel->label << std::endl;
  char **label = rel->label;
    char *name = *label;
  llvm::GlobalVariable *global = TheModule->getGlobalVariable(name);
  llvm::Constant *value = global->getInitializer();
  return llvm::ConstantExpr::getPtrToInt(global, destTy);
}

llvm::Constant *EmitDoubleInitialication(llvm::Type *destTy, Type *IntType, char *buf, int offset, Relocation *rel) {
  int size = IntType->size;
  if (!buf) {
    return llvm::Constant::getNullValue(destTy);
  }
  if (!rel) {
    double dVal = *(double *)(buf+offset);
    return llvm::ConstantFP::get(CGM().getLLVMContext(), llvm::APFloat(dVal));
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
      constant = init_struct(static_cast<llvm::StructType *>(varType), ctype, buf + offset);
      break; 
    case TY_PTR:
      constant = EmitPointerInitialization(static_cast<llvm::PointerType *>(varType), ctype, buf, offset, rel);
      break;
    case TY_FLOAT:
      constant = EmitFloatInitialication(varType, ctype, buf, offset, rel);
      break;
    case TY_DOUBLE:
      constant = EmitDoubleInitialication(varType, ctype, buf, offset, rel);
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

  output << "createGlobalVar: " << name << std::endl;
  llvm::Type *DesiredType = get_init_type(ctype);
  TheModule->getOrInsertGlobal(name, DesiredType);
  llvm::GlobalVariable *gVar = TheModule->getNamedGlobal(name);
  gVar->setAlignment(llvm::MaybeAlign(yucNode->align));
  if (!yucNode->is_static) {
  	gVar->setDSOLocal(true);
  }
  if (yucNode->is_definition) {
    llvm::Constant *initializer = create_initializer(DesiredType, yucNode);
    putGlobalVar(yucNode, initializer);
    gVar->setInitializer(initializer);
  }
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

static llvm::Type *gen_return_type(Type *return_ty) {
  if (return_ty->kind == TY_STRUCT) {
    return struct_to_return(return_ty);
  }
  llvm::Type *retTy = yuc2LLVMType(return_ty);
  return retTy;
}

static llvm::FunctionType * buildFunctionType(Obj *funcNode) {
  std::vector<llvm::Type *> types;
  Type *funcType = funcNode->ty;
  for (Type *paramType = funcType->params; paramType; paramType = paramType->next) {
    llvm::Type *type = yuc2LLVMType(paramType);
    types.push_back(type);
  }
  llvm::Type *RetTy = gen_return_type(funcType->return_ty);
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
  std::string funcName = funcNode->name;
  output << "createFunc: " << funcName << std::endl;
  llvm::Value *foo = find_var(funcNode);
  if (foo) {
    return static_cast<llvm::Function *>(foo);
  }
  llvm::FunctionType *funcType = buildFunctionType(funcNode);
  llvm::GlobalValue::LinkageTypes linkageType = yuc2LinkageType(funcNode);
  llvm::Function *fooFunc = llvm::Function::Create(funcType, linkageType, funcName, TheModule.get());
  if(!funcNode->is_static) {
    fooFunc->setDSOLocal(true);
  }
  push_var(funcNode, fooFunc);
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

static bool is_builtin_name(std::string &name) {
  return name == "__alloca_size__" 
      || name == "__va_area__";
}

static void print_type(Type *type) {
  if (!type) {
    return;
  }
  output << "Type kind: " << ctypeKindString(type->kind) 
            << " align: " << type->align
            << std::endl;
}

static void print_obj(Obj *obj) {
  if (!obj) {
    return;
  }
  output << "Obj name: " << obj->name 
            << " align: " << obj->align
            << std::endl;
  print_type(obj->ty);
}

static void alloca_local_var(Obj *local) {
  output << "alloca_local_var: " << std::endl;
  if (!local) {
    return;
  }
  print_obj(local);
  Type *ty = local->ty;
  if (ty->kind == TY_VLA) {
    return;
  }
  std::string varName = local->name;
  Obj *var = local;
  int align = get_align(var);
  llvm::Type *localType = getTypeForArg(ty);
  llvm::AllocaInst *localAddr = Builder->CreateAlloca(localType, nullptr);
  localAddr->setAlignment(llvm::Align(align));
  push_var(local, localAddr);
}

static void alloca_params(Obj *func) {
  output << "alloca_params: " << std::endl;
  if (!func) {
    return;
  }
  for (Obj *param = func->params; param; param = param->next) {
    alloca_local_var(param);
  }  
}

static void alloca_local_vars(Obj *func) {
  output << "alloca_local_vars: " << std::endl;
  std::vector<Obj *> locals;
  for (Obj *local = func->locals; local; local = local->next) {
    std::string var_name = local->name;
    if (is_builtin_name(var_name)) {
      break;
    }
    locals.push_back(local);
  }

  for (std::vector<Obj *>::reverse_iterator iter = locals.rbegin();
    iter != locals.rend(); iter++) {
    Obj *local = (*iter);
    llvm::Value *local_value = find_var(local);
    if (local_value) {
      continue;
    }
    alloca_local_var(local);
  }
}

static void store_args(llvm::Function *Func, Obj *func) {
   // store args
  Obj *param = func->params;
  llvm::Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, param = param->next) {
    llvm::Value *arg_addr = find_var(param);
    TypeKind typeKind = param->ty->kind;
    llvm::Value *fnArg = AI;
    if (typeKind == TY_BOOL) {
      fnArg = Builder->CreateZExt(AI, Builder->getInt8Ty());
    }
    Builder->CreateStore(fnArg, arg_addr);
  } 
}

static void prepare_args_and_locals(llvm::Function *Func, Obj *funcNode) {
  if (funcNode->retCount > 1) {
    llvm::Type *returnType = Func->getReturnType();
    allocatedRetValue = Builder->CreateAlloca(returnType, nullptr);
  }

  alloca_params(funcNode);
  alloca_local_vars(funcNode);
  store_args(Func, funcNode);

  if (funcNode->ty->return_ty->kind == TY_STRUCT) {
    llvm::Type *returnType = Func->getReturnType();
    llvm::Value *ret_alloca = Builder->CreateAlloca(returnType, nullptr);
    // todo add it in parse phase
    // push_var(".ret_alloca", ret_alloca);
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
  prepare_args_and_locals(Fn, funcNode);

  if (defaultAlloca) {
    Builder->CreateStore(Builder->getInt32(0), defaultAlloca);
  }
}

static void buildFunctionBody(llvm::Function *Func, Obj *funcNode) {
  output << "buildFunctionBody " << std::endl;
  gen_stmt(funcNode->body);
}

static llvm::Function *declare_func(Obj *funcNode) {
  llvm::Function *fooFunc = createFunc(funcNode);
  return fooFunc;
}

static void define_func(Obj *funcNode) {
  llvm::Function *fooFunc = declare_func(funcNode);
  // SetLLVMFunctionAttributes(fooFunc);
  if (!funcNode->is_definition) {
    output << "buildFunction: declaration" << std::endl;
    return;
  }
  output << "buildFunction: start\n";
  enter_scope();
  StartFunction(fooFunc, funcNode);
  buildFunctionBody(fooFunc, funcNode);
  FinishFunction(fooFunc, funcNode);
  leave_scope();
  llvm::verifyFunction(*fooFunc);
  output << "buildFunction: end\n";
}

static void emit_function(Obj *fn) {
  Obj *prev = nullptr;
  Obj *cur = fn;
  while(cur) {
    if (cur->ty->kind != TY_FUNC) {
      cur = cur->next;
    } else if (!cur->is_definition) {
      declare_func(cur);
      cur = cur->next;
    } else {
      Obj *tmp = cur->next;
      cur->next = prev;
      prev = cur;
      cur = tmp;
    }
  }
  for (Obj *cur = prev; cur; cur = cur->next) {
    define_func(cur);
  }
}

void gen_ir(Obj *prog, const std::string &filename) {
	Obj *annonP, *namedP;
	Obj **annon = &annonP, **named = &namedP;
	for (Obj *cur = prog; cur; cur = cur->next) {
		std::string name = cur->name;
    // output << "gen ir: " << name << std::endl;
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
  emit_function(namedP);
	TheModule->print(llvm::outs(), nullptr);
  // TheModule->dump();
}

