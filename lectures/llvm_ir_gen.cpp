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
#define DUMP_NODE 1
#define DUMP_SCOPE 1

//
//= llvm framework utils
//

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;

static void InitializeModule(const std::string &filename) {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>(filename, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}

static llvm::LLVMContext &getLLVMContext() {
  return TheModule->getContext();
}

//
//= block scope utils
//

typedef struct BlockScope {
  BlockScope *next;

  std::map<std::string, llvm::Value *> vars;
  std::map<std::string, llvm::StructType *> tags;
  std::map<std::string, llvm::Value *> constants;
} BlockScope;

static BlockScope *scope = new BlockScope();

static void enter_scope(void) {
  BlockScope *sc = new BlockScope();
  sc->next = scope;
  scope = sc;
  if (DUMP_SCOPE) {
    std::cout << "enter_scope: "<< std::endl;
  }
}

static void leave_scope(void) {
  scope = scope->next;
  if (DUMP_SCOPE) {
    std::cout << "leave_scope: " << std::endl;
  }
}

static std::string get_scope_name(Obj *var) {
  std::string var_name = var->name;
  if (var->is_local) {
    int scope_level = var->scope_level;
    var_name.append(".");
    var_name.append(std::to_string(scope_level));
  }
  return var_name;
}

static void push_var(Obj *var, llvm::Value *v) {
  std::string var_name = get_scope_name(var);
  if (DUMP_SCOPE) {
    std::cout << "push_var: " << var_name << std::endl;
  }
  scope->vars[var_name] = v;
}

static llvm::Value *find_var(Obj *var) {
  std::string var_name = get_scope_name(var);
  if (DUMP_SCOPE) {
    std::cout << "find_var: " << var_name << std::endl;
  }
  for (BlockScope *sc = scope; sc; sc = sc->next) {
    llvm::Value *v = sc->vars[var_name];
    if (v) {
      return v;
    }
  }
  return nullptr;
}

//
//= debug utils
//

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

static void dump_node(std::string tag, Node *node) {
  if (!node || !DUMP_NODE) {
    return;
  }
  std::cout << tag
            << "=>Node type: " << node_kind_info(node->kind)
            << std::endl;
}

static void dump_obj(Obj *obj) {
  if (!DUMP_OBJ) {
    return;
  }
  for (Obj *cur = obj; cur; cur = cur->next) {
    print_obj(cur);
  }
}

//
//= ast parse utils
//

static bool isAnnonVar(std::string &name) {
  int index = name.find(".L..");
  return index == 0;
}

static int get_align(Obj *var) {
  int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;
  return align;
}

static bool is_builtin_name(std::string &name) {
  return name == "__alloca_size__" 
      || name == "__va_area__";
}

static llvm::Type *create_type(Type *ty) {
  llvm::Type *type;
  switch(ty->kind) {
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
  if (isAnnonVar(var_name)) {
    return;
  }
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

//
// llvm ir emit utils
//

// load value for a type
static llvm::Value* emit_load(Type *ty, llvm::Value *addr) {
  llvm::Type *type = create_type(ty);
  llvm::Value *V = Builder->CreateLoad(type, addr);
  if (ty->kind == TY_BOOL) {
    // bool is i8 in memory
    V = Builder->CreateTrunc(V, Builder->getInt1Ty());
  }
  return V;
}

//
//= emit statement and expression llvm ir
//
static void gen_stmt(Node *node);
static void gen_block(Node *node);
static void gen_block_item(Node *node);

static llvm::Value *gen_expr(Node *node);
static llvm::Value *gen_add(Node *node);
static llvm::Value *gen_sub(Node *node);
static llvm::Value *gen_mul(Node *node);
static llvm::Value *gen_div(Node *node);
static llvm::Value *gen_mod(Node *node);
static llvm::Value *gen_bitand(Node *node);
static llvm::Value *gen_bitor(Node *node);
static llvm::Value *gen_bitxor(Node *node);
static llvm::Value *gen_bitnot(Node *node);
static llvm::Value *gen_shl(Node *node);
static llvm::Value *gen_shr(Node *node);
static llvm::Value *gen_cast(Node *node);
static llvm::Value *gen_var_value(Node *node);
static llvm::Value *gen_relational(Node *node);

static llvm::Value *gen_null(Node *node);
static llvm::Value *emit_cast(llvm::Value *V, Type *from, Type *to);

static llvm::Value *(*gen_table[ND_DUMMY])(Node *node) = {
  [ND_NULL_EXPR] = gen_null,
  [ND_CAST] = gen_cast,
  [ND_VAR] = gen_var_value,

  // math operation
  [ND_ADD] = gen_add,
  [ND_SUB] = gen_sub,
  [ND_MUL] = gen_mul,
  [ND_DIV] = gen_div,
  [ND_MOD] = gen_mod,

  // bitwise operation
  [ND_BITAND] = gen_bitand,
  [ND_BITOR] = gen_bitor,
  [ND_BITXOR] = gen_bitxor,
  [ND_BITNOT] = gen_bitnot,
  [ND_SHL] = gen_shl,
  [ND_SHR] = gen_shr,

  // relational operation
  [ND_LT] = gen_relational,
  [ND_LE] = gen_relational,
  [ND_GT] = gen_relational,
  [ND_GE] = gen_relational,
  [ND_EQ] = gen_relational,
  [ND_NE] = gen_relational,
};

// cast tables
enum { B8, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80, PTR };

static int get_type_id(Type *ty) {
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
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateTrunc(V, targetTy);
};

static llvm::Value *i8i64(llvm::Value *V, Type *to_type) 
{
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateSExt(V, targetTy);
};

static llvm::Value *u8i64(llvm::Value *V, Type *to_type) 
{
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateZExt(V, targetTy);
};

// signed int to double long
static llvm::Value *i8f80(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateSIToFP(V, targetTy);
};

// unsigned int to double long
static llvm::Value *u8f80(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateUIToFP(V, targetTy);
};

// double long to signed int
static llvm::Value *f80i8(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateFPToSI(V, targetTy);
};

// double long to unsigned int
static llvm::Value *f80u8(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateFPToUI(V, targetTy);
};

static llvm::Value *ptrptr(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateBitCast(V, targetTy);
}

static llvm::Value *ptrint(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreatePtrToInt(V, targetTy);
}

static llvm::Value *intptr(llvm::Value *V, Type *to_type) {
  if (V->getType()->getIntegerBitWidth() < 64) {
    V = Builder->CreateZExt(V, Builder->getInt64Ty());
  }
  llvm::Type *targetTy = create_type(to_type);
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
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateZExt(V, targetTy);
}

static llvm::Value *fpext(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
  return Builder->CreateFPExt(V, targetTy);
}

static llvm::Value *fptrunc(llvm::Value *V, Type *to_type) {
  llvm::Type *targetTy = create_type(to_type);
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

// relation operation tables
enum { LT, LE, GT, GE, EQ, NE };
enum { UNSIGNED, SIGNED};
enum { Int, Float };

static int get_relation_id(Node *node) {
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

static int get_type_category(Type *ty) {
  return is_flonum(ty) ? Float : Int;
}

static int get_sign_id(Type *ty) {
  return ty->is_unsigned ? UNSIGNED : SIGNED;
}

// operation Predicate Table [Relation][Signed][Integer]
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


// emit num + num
static llvm::Value *gen_math_add(Type *result_ty, llvm::Value *L, llvm::Value *R) {
  if (is_flonum(result_ty)) {
    return Builder->CreateFAdd(L, R);
  } 

  if (result_ty->is_unsigned) {
    return Builder->CreateAdd(L, R);
  }
  return Builder->CreateNSWAdd(L, R);
}

// emit ptr + num or addr + num
static llvm::Value *gen_addr_add_offset(Node *addr, llvm::Value *L, llvm::Value *R) {
  return nullptr;
}

// emit add operation
static llvm::Value *gen_add(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  if (node->o_kind == PTR_NUM) {
    // ptr + num -> GEP(ptr, num)
    // arr + num -> GEP(arr, 0, num)
    return gen_addr_add_offset(node->lhs, L, R);
  }
  return gen_math_add(node->ty, L, R);
}

// emit ptr/addr - num
static llvm::Value *gen_addr_sub_offset(Node *addr, llvm::Value *L, llvm::Value *R) {
  return nullptr;
}

// emit ptr - ptr
static llvm::Value *gen_addr_sub(Node *addr, llvm::Value *L, llvm::Value *R) {
  return nullptr;
}

// emit num - num
static llvm::Value *gen_math_sub(Type *result_ty, llvm::Value *L, llvm::Value *R) {
  if (is_flonum(result_ty)) {
    return Builder->CreateFSub(L, R);
  } 

  if (result_ty->is_unsigned) {
    return Builder->CreateSub(L, R);
  }
  return Builder->CreateNSWSub(L, R);
}

// emit sub operation
static llvm::Value *gen_sub(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  if (node->o_kind == PTR_PTR) {
    return gen_addr_sub(node, L, R);
  }

  if (node->o_kind == PTR_NUM) {
    return gen_addr_sub_offset(node, L, R);
  }

  return gen_math_sub(node->ty, L, R);
}

// emit add operation
static llvm::Value *gen_mul(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  if (is_flonum(node->ty)) {
    return Builder->CreateFMul(L, R);
  }
  if (node->ty->is_unsigned) {
    return Builder->CreateMul(L, R);
  } 
  return Builder->CreateNSWMul(L, R);
}

// emit div
static llvm::Value *gen_div(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  if (is_flonum(node->ty)) {
    return Builder->CreateFDiv(L, R);
  }
  if (node->ty->is_unsigned) {
    return Builder->CreateUDiv(L, R);
  } 
  return Builder->CreateSDiv(L, R);
}

// emit mode
static llvm::Value *gen_mod(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);  
  if (node->ty->is_unsigned) {
    return Builder->CreateURem(L, R);
  } 
  return Builder->CreateSRem(L, R);
}

// emit bit and
static llvm::Value *gen_bitand(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);  
  return Builder->CreateAnd(L, R);
}

// emit bit or
static llvm::Value *gen_bitor(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);  
  return Builder->CreateOr(L, R);
}

// emit bit xor
static llvm::Value *gen_bitxor(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  return Builder->CreateXor(L, R);
}

// emit bit not
static llvm::Value *gen_bitnot(Node *node) {
  llvm::Value *operand = gen_expr(node->lhs);
  return Builder->CreateXor(operand, -1);
}

// emit shift left
static llvm::Value *gen_shl(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);  
  return Builder->CreateShl(L, R);
}

// emit sheft right
static llvm::Value *gen_shr(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);  
  if (node->ty->is_unsigned) {
    return Builder->CreateLShr(L, R);
  } 
  return Builder->CreateAShr(L, R);
}


// get predicate
static llvm::CmpInst::Predicate get_predicate(Node *node, Type *nodeType) {
  int relationId = get_relation_id(node);
  int signId = get_sign_id(nodeType);
  int catagory = get_type_category(nodeType);
  return predicate_table[relationId][signId][catagory];
}

// emit relational ir
static llvm::Value *gen_relational(Node *node) {
  llvm::Value *L = gen_expr(node->lhs);
  llvm::Value *R = gen_expr(node->rhs);
  llvm::CmpInst::Predicate predicate = get_predicate(node, node->lhs->ty);
  llvm::Value *V = Builder->CreateCmp(predicate, L, R);
  if (node->eval_as_bool) {
    return V;
  }
  return emit_cast(V, ty_bool, ty_int);
}

// declaration or statement
static void gen_block_item(Node *node) {
  Node *stmt = node->body;
  while(stmt) {
    gen_stmt(stmt);
    stmt = stmt->next;
  }
}

// emit statemnt in block 
static void gen_block(Node *node) {
  enter_scope();
  gen_block_item(node);
  leave_scope();
}

// emit cast value by type
static llvm::Value *emit_cast(llvm::Value *V, Type *from, Type *to) {
  int t1 = get_type_id(from);
  int t2 = get_type_id(to);
  if (cast_table[t1][t2]) {
    return cast_table[t1][t2](V, to);
  }
  return V;
}

// emit cast expression
static llvm::Value *gen_cast(Node *node) {
  llvm::Value *V = gen_expr(node->lhs);
  Type *from_type = node->lhs->ty;
  Type *to_type = node->ty;
  return emit_cast(V, from_type, to_type);
}

// emit variable address
static llvm::Value *gen_var_addr(Obj *var) {
  if (var->is_local) {
    // get local variable
    return find_var(var);
  }
  return nullptr;
}

// emit node adderss
static llvm::Value *gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    return gen_var_addr(node->var);
  }
}

// emit load var value
static llvm::Value *gen_var_value(Node *node) {
  Obj *var = node->var;
  llvm::Value *addr = gen_addr(node);
  Type *ty = var->ty;
  return emit_load(ty, addr);
}

// emit null
static llvm::Value *gen_null(Node *node) {
  return nullptr;
}

// emit all expression
static llvm::Value *gen_expr(Node *node) {
  dump_node("gen_expr", node);
  llvm::Value *V = nullptr;
  if (!node) {
    return V;
  }
  V = gen_table[node->kind](node);
  return V;
}

static void gen_stmt(Node *node) {
  dump_node("gen_stmt", node);
  switch (node->kind)
  {
  case ND_BLOCK_ITEM:
    gen_block_item(node);
    break;
  case ND_BLOCK:
    gen_block(node);
    break;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    break;
  default:
    std::cout << "Unimplemented kind: " << node->kind << std::endl;
  }
}

//
//= emit function llvm ir
//

static llvm::Type *create_return_type(Type *return_ty) {
  llvm::Type *retTy = create_type(return_ty);
  return retTy;
}

static std::vector<llvm::Type *> create_params_type(Type *funcType) {
  std::vector<llvm::Type *> types;
  for (Type *paramType = funcType->params; paramType; paramType = paramType->next) {
    llvm::Type *type = create_type(paramType);
    types.push_back(type);
  }
  return types;
}

static llvm::FunctionType * create_prototype(Obj *funcNode) {
  assert(funcNode->ty->kind == TY_FUNC);
  Type *funcType = funcNode->ty;

  // create function input type
  std::vector<llvm::Type *> types = create_params_type(funcType);
  // create function output type
  llvm::Type *RetTy = create_return_type(funcType->return_ty);
  // check is variable arg function
  bool isVarArg = funcType->is_variadic;
  if (types.empty()) {
    isVarArg = false;
  }
  // get prototype
  llvm::FunctionType *functionType = llvm::FunctionType::get(RetTy, types, isVarArg);
  return functionType;
}

static llvm::Function *declare_func(Obj *funcNode) {
  assert(funcNode->ty->kind == TY_FUNC);

  std::string funcName = funcNode->name;
  llvm::FunctionType *funcType = create_prototype(funcNode);
  llvm::GlobalValue::LinkageTypes linkageType = create_linkage_type(funcNode);
  // register function declaration
  llvm::Function *func = llvm::Function::Create(funcType, linkageType, funcName, TheModule.get());
  return func;
}

static void alloca_local_var(Obj *local) {
  if (!local) {
    return;
  }

  llvm::Type *localType = create_type(local->ty);
  llvm::AllocaInst *localAddr = Builder->CreateAlloca(localType, nullptr);
  int align = get_align(local);
  localAddr->setAlignment(llvm::Align(align));
  push_var(local, localAddr);
}

static void alloca_params(Obj *param) {
  while(param) {
    alloca_local_var(param);
    param = param->next;
  }
}

static void alloca_local_vars(Obj *local) {
  if(!local) {
    return;
  }

  alloca_local_vars(local->next);

  std::string var_name = local->name;
  if (is_builtin_name(var_name)) {
    return;
  }

  llvm::Value *local_value = find_var(local);
  if (local_value) {
    return;
  }
  alloca_local_var(local);
}

static void store_args(llvm::Function *Func, Obj *func) {
  // 获取形参地址
  Obj *param = func->params;
  // for循环获取实参对应的llvm::Value,store到栈空间中
  for(llvm::Function::arg_iterator AI = Func->arg_begin(); 
      AI != Func->arg_end(); ++AI, param = param->next) {
    // 获取实参在栈上分配的地址
    llvm::Value *arg_addr = find_var(param);
    // 获取实参对应的llvm::Value
    llvm::Value *fnArg = AI;
    TypeKind typeKind = param->ty->kind;
    // 对bool值进行特殊处理
    if (typeKind == TY_BOOL) {
      fnArg = Builder->CreateZExt(AI, Builder->getInt8Ty());
    }
    // store value
    Builder->CreateStore(fnArg, arg_addr);
  } 
}

// 定义prepare_args_and_locals，实现函数参数和局部变量的栈空间分配操作
static void prepare_args_and_locals(llvm::Function *Func, Obj *funcNode) {
  // 栈上分配参数
  alloca_params(funcNode->params);
  // 栈上分配局部变量
  alloca_local_vars(funcNode->locals);
  // 存储实参
  store_args(Func, funcNode);
}

static void start_function(llvm::Function *Fn, Obj *funcNode) {
  const char *name = Fn->getName().data();
  // create function init basicblock
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(getLLVMContext(), "", Fn);
  Builder->SetInsertPoint(entry);
  // 栈上分配实参和局部变量
  prepare_args_and_locals(Fn, funcNode);
}

static void build_function_body(llvm::Function *Func, Obj *funcNode) {
  gen_stmt(funcNode->body);
}

static void finish_function(llvm::Function *Func, Obj *funcNode) {

  Builder->CreateRet(Builder->getInt32(-1024));
}

static void define_func(Obj *funcNode) {
  assert(funcNode->is_definition);
  enter_scope();
  llvm::Function *fooFunc = declare_func(funcNode);  
  start_function(fooFunc, funcNode);
  build_function_body(fooFunc, funcNode);
  finish_function(fooFunc, funcNode);
  llvm::verifyFunction(*fooFunc);
  leave_scope();
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
  emit_data(prog);
  emit_function(prog);

  TheModule->print(llvm::outs(), nullptr);
}