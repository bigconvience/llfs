#include "yuc.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Constants.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include <algorithm>

using namespace llvm;
using namespace yuc;

static Type *yuc2LLVMType(CType *ctype);

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
// https://llvm.org/doxygen/IRBuilder_8h_source.html
static std::unique_ptr<IRBuilder<>> Builder;
static int stmt_level = 0;
static int stmt_count = 0;
static std::ofstream output("./test2/ast_ir.out");
static std::map<int, Value*> LocallAddress;
static Constant *buffer2Constants(Type *Ty, CType *ctype, CRelocation *rel, char *buf, int offset);
static std::map<string, Ast*> annonVar;
static std::map<string, llvm::GlobalVariable*> annonGlobalVar;
static std::map<string, llvm::GlobalVariable*> strLiteralCache;

static bool isAnnonVar(std::string &name) {
  int index = name.find(".L..");
  return index == 0;
}

class CodeGenModule {
private:
  llvm::Module &TheModule;
  llvm::LLVMContext &VMContext;

public:
  CodeGenModule(llvm::Module &M);
  ~CodeGenModule();
  
  llvm::GlobalVariable *GetAddrOfConstantCString(const std::string &Str, const char *GlobalName);

  llvm::LLVMContext &getLLVMContext() { return VMContext; }
  llvm::Module &getModule() { return TheModule; }

};

static llvm::GlobalVariable *
GenerateStringLiteral(llvm::Constant *C, llvm::GlobalValue::LinkageTypes LT,
                      CodeGenModule &CGM, StringRef GlobalName,
                      int Alignment) {
  unsigned AddrSpace = 0;
  llvm::Module &M = CGM.getModule();
  // Create a global variable for this string
  auto *GV = new llvm::GlobalVariable(
      M, C->getType(), true, LT, C, GlobalName,
      nullptr, llvm::GlobalVariable::NotThreadLocal, AddrSpace);
  GV->setAlignment(MaybeAlign(Alignment));
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  if (GV->isWeakForLinker()) {
    GV->setComdat(M.getOrInsertComdat(GV->getName()));
  }
  return GV;
}

CodeGenModule::CodeGenModule(llvm::Module &M)
  : TheModule(M), VMContext(M.getContext()) {
  // Initialize the type cache.
  llvm::LLVMContext &LLVMContext = M.getContext();
}

CodeGenModule::~CodeGenModule() {}

llvm::GlobalVariable *CodeGenModule::GetAddrOfConstantCString(const std::string &Str, const char *GlobalName) {
  if (strLiteralCache[Str]) {
    return strLiteralCache[Str];
  }

  StringRef StrWithNull(Str.c_str(), Str.size() + 1);
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

static std::unique_ptr<CodeGenModule> ModuleBuilder;

CodeGenModule &CGM() {
  return *ModuleBuilder;
}


llvm::Type *getPaddingType(int PadSize) {
  llvm::Type *Ty = Builder->getInt8Ty();
  if (PadSize > 1) {
    Ty = llvm::ArrayType::get(Ty, PadSize);
  }
  return Ty;
}

llvm::Constant *getPadding(int PadSize) {
  llvm::Type *Ty = getPaddingType(PadSize);
  return llvm::UndefValue::get(Ty);
}

static string buildSeperator(int count, string target) {
  string result = "";
  for (int i = 1; i < count; i++) {
    result.append("\t");
  }
  result.append(target);
  return result;
}

static void gen_addr(CNode *node) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_addr")
      << " node kind:" << node->kind << endl;
  cur_level++;

  switch(node->kind) {
    case CNode::CNodeKind::ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
  }
  --stmt_level;
}

static void cast(CType *from, CType *to) {

}

static void gen_expr(CNode *node) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_expr start, kind:") 
  << node->kind <<  endl; 
  cur_level++;
  switch(node->kind) {
    case CNode::CNodeKind::ND_NULL_EXPR:
      output << buildSeperator(cur_level, "ND_NULL_EXPR:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_COMMA:
      output << buildSeperator(cur_level, "ND_COMMA:") << node->kind << endl;
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      break;
    case CNode::CNodeKind::ND_COND:
      output << buildSeperator(cur_level, "ND_COND:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_ASSIGN:
      output << buildSeperator(cur_level, "ND_ASSIGN:") << node->kind << endl;
      gen_addr(node->lhs);
      gen_expr(node->rhs);

      break;
    case CNode::CNodeKind::ND_NUM:
      output << buildSeperator(cur_level, "ND_NUM:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_MEMZERO:
      output << buildSeperator(cur_level, "ND_MEMZERO:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
    case CNode::CNodeKind::ND_CAST:
      output << buildSeperator(cur_level, "ND_CAST:") << node->kind << endl;
      gen_expr(node->lhs);
      cast(node->lhs->type, node->type);
      break;
    case CNode::CNodeKind::ND_ADD:
      output << buildSeperator(cur_level, "ND_ADD:") << node->kind << endl;
      break;

  }
  stmt_level--;
  cur_level--;
  output << buildSeperator(cur_level, "gen_expr end") << endl; 
}

static void gen_stmt(CNode *node) {
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  output << buildSeperator(level, "gen_stmt start ==> ") << cur_count << endl;
  switch(node->kind) {
    case CNode::CNodeKind::ND_BLOCK: // 32
      output << buildSeperator(level+1, "ND_BLOCK:") << node->kind << "\n"; 
      for (CNode *n = node->body; n; n = n->next) {
        gen_stmt(n);
      }
      break;
    case CNode::CNodeKind::ND_EXPR_STMT:
      output << buildSeperator(level+1, "ND_EXPR_STMT:")<< node->kind << "\n"; 
      gen_expr(node->lhs);
      break;

    case CNode::CNodeKind::ND_RETURN:
      output << buildSeperator(level+1, "ND_RETURN:") << node->kind << "\n"; 
      break;
    default:
      output << buildSeperator(level+1, "gen_stmt unknow kind: ") << node->kind << endl;
      break;
  }
  output << buildSeperator(level, "gen_stmt end <<<=== ") <<  cur_count << endl;
  --stmt_level;
}

static void ComputeRecordLayout(CType *ctype, llvm::StructType *Ty) {
  cout << "ComputeRecordLayout" << endl;
  llvm::SmallVector<llvm::Type *, 16> Types;
  Types.reserve(ctype->memberCount);
  CMember *member = ctype->union_field;
  if (member) {
    Types.push_back(yuc2LLVMType(member->ty));
    Ty->setBody(Types);
    return;
  }
  
  for (CMember *member = ctype->members; member; member = member->next) {
    Types.push_back(yuc2LLVMType(member->ty));
  }
  Ty->setBody(Types);
}

static llvm::ArrayType *yuc2ArrayType(CType *ctype) {
  Type *base = yuc2LLVMType(ctype->base);
  int array_len = ctype->array_len;
  llvm::ArrayType *type = llvm::ArrayType::get(base, array_len);
  return type;
}

static Type *yuc2PointerType(CType *ctype) {
  Type *base = yuc2LLVMType(ctype->base);
  Type *type = PointerType::get(base, 0);
  return type;
}

static llvm::StructType *yuc2StructType(CType *ctype) {
  cout << "yuc2StructType" << endl;
  llvm::SmallVector<llvm::Type *, 16> Types;
  Types.reserve(ctype->memberCount);
  CMember *member = ctype->union_field;
  llvm::StructType *type;
  int DesiredSize = ctype->size;
  cout << "yuc2StructType DesiredSize:" << DesiredSize << endl;
  // union
  if (member) {
    int Size = member->ty->size;
    cout << "union member idx: " << member->idx << " size: " << Size << endl;
    
    Types.push_back(yuc2LLVMType(member->ty));
    if (DesiredSize > Size) {
      int PadSize = DesiredSize - Size;
      cout << "PadSize: " << PadSize << endl;
      llvm::Type *paddingType = getPaddingType(PadSize);
      Types.push_back(paddingType);
    }

    if (member->idx) {
      type = llvm::StructType::get(*TheContext, Types, false);
    } else {
      type = llvm::StructType::create(*TheContext, Types, "", false);
    }
    cout << "final type: " << Types.size() << endl;
    return type;
  }
  // struct
  for (CMember *member = ctype->members; member; member = member->next) {
    Types.push_back(yuc2LLVMType(member->ty));
  }
  if (ctype->is_typedef) {
    type = llvm::StructType::get(*TheContext, Types, false);
  } else {
    type = llvm::StructType::create(*TheContext, Types, "", false);
  }
  return type;
}

void addRecordTypeName(CType *ctype, llvm::StructType *Ty, StringRef suffix) {
  SmallString<256> TypeName;
  llvm::raw_svector_ostream OS(TypeName);
  string kindName = CType::ctypeKindString(ctype->kind);
  OS << kindName << ".";

  OS << "anon";
  if (!suffix.empty()) {
    OS << suffix;
  }
  Ty->setName(OS.str());
}

static llvm::StructType *ConvertRecordDeclType(CType *ctype) {
  cout << "ConvertRecordDeclType" << endl;
  llvm::StructType *DesiredType = yuc2StructType(ctype);
  addRecordTypeName(ctype, DesiredType, "");
  return DesiredType;
}

static Type *yuc2LLVMType(CType *ctype) {
  cout << "yuc2LLVMType start kind: "
      << CType::ctypeKindString(ctype->kind) << endl;
  Type *type;
  switch (ctype->kind) {
    case CType::TY_CHAR:
      type = Builder->getInt8Ty();
      break;
    case CType::TY_SHORT:
      type = Builder->getInt16Ty();
      break;
    case CType::TY_INT:
      type = Builder->getInt32Ty();
      break;
    case CType::TY_LONG:
      type = Builder->getInt64Ty();
      break;
    case CType::TY_PTR:
      type = yuc2PointerType(ctype);
      break;
    case CType::TY_ARRAY:
      type = yuc2ArrayType(ctype);
      break;
    case CType::TY_UNION:
    case CType::TY_STRUCT:
      type = ConvertRecordDeclType(ctype);
      break;
    default:
      type = Builder->getInt32Ty();
      break;
  }
  return type;
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

llvm::Constant *EmitArrayInitialization(llvm::ArrayType *Ty, CType *ArrayType, char *buf, int offset, CRelocation *rel) {
  SmallVector<llvm::Constant *, 16> Elts;
  unsigned NumElements = ArrayType->array_len;
  Elts.reserve(NumElements);
  int sz = ArrayType->base->size;

  std::cout << "EmitArrayInitialization： " << NumElements 
    << " size: " <<  sz
    << endl;
  
  Type *CommonElementType = Ty->getElementType();
  CType *baseTy = ArrayType->base;
  llvm::Constant *C = nullptr;
  if (!rel) {
     for(int i = 0; i < NumElements; i++) {
      C = buffer2Constants(CommonElementType, baseTy, NULL, buf, offset + sz * i);
      Elts.push_back(C);
    }   
  } else {
    int i = 0;
    while(rel) {
      std::cout << "label: " << *rel->label << std::endl;
      C = buffer2Constants(CommonElementType, baseTy, rel, buf, offset + sz * i);
      Elts.push_back(C);
      rel = rel->next;
      i++;
    } 
  }

  return llvm::ConstantArray::get(
        llvm::ArrayType::get(CommonElementType, NumElements), Elts);
 }
 
llvm::Constant *EmitRecordInitialization(llvm::StructType *Ty, CType *ctype, char *buf, int offset) {
  SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = ctype->memberCount;
  Elements.reserve(NumElements);
  std::cout << "EmitRecordInitialization NumElements： " << NumElements
    << endl;
  int index = 0; 
  for (CMember *member = ctype->members; member; member = member->next) {
    // Type *Ty = yuc2LLVMType(member->ty);
    Type *memberTy = Ty->getTypeAtIndex(index++);
    llvm::Constant *constantValue = buffer2Constants(memberTy, member->ty, NULL, buf, offset + member->offset);
    Elements.push_back(constantValue);
  }

  return llvm::ConstantStruct::get(Ty, Elements);
}  

llvm::Constant *EmitUnionInitialization(llvm::StructType *Ty, CType *ctype, char *buf, int offset) {
  int DesiredSize = ctype->size;
  int AlignedSize = ctype->align; 
  SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = ctype->memberCount;
  std::cout << "EmitUnionInitialization NumElements： " << NumElements 
    << " DesiredSize: " <<  DesiredSize
    << " AlignedSize: " <<  AlignedSize 
    << endl;
  Elements.reserve(NumElements);
  bool Packed = false;
  CMember *member = ctype->union_field;
  if (member) {
    int Size = member->ty->size;
    std::cout << "EmitUnionInitialization union Size " << Size << endl;
    Type *Ty = yuc2LLVMType(member->ty);
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
  llvm::Constant *getOffset(long offset) {
    return llvm::ConstantInt::get(Builder->getInt64Ty(), offset);
  }

/// Apply the value offset to the given constant.
llvm::Constant *applyOffset(llvm::Constant *C, long offset) {
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

llvm::Constant *EmitPointerInitialization(llvm::PointerType *Ty, CType *ctype, char *buf, int offset, CRelocation *rel) {
  cout << "EmitPointerInitialization: size: " << ctype->size << endl;

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
    cout << " addend: " << addend << " name:" << name << endl;
    GlobalVariable *global;
    string nameStr = name;
    if (isAnnonVar(nameStr)) {
      global = annonGlobalVar[nameStr];
      cout << " annonGlobalVar: " << global->isConstant() << endl;
    } else {
      global = TheModule->getGlobalVariable(name);
    }
    
    Constant *constant = global->getInitializer();
    Base = global;
    BaseValueTy = constant->getType();
    BaseValueTy->dump();
    IndexValues[0] = llvm::ConstantInt::get(Builder->getInt32Ty(), Indices[0]);

    CType::CTypeKind baseTypeKind = ctype->base->kind;
    std::cout << " base kind:" << CType::ctypeKindString(baseTypeKind);
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
        std::cout << " indice: " << indice;

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

llvm::Constant *EmitIntegerInitialication(llvm::Type *destTy, CType *IntType, char *buf, int offset, CRelocation *rel) {
  int size = IntType->size;
  if (!rel) {
    return llvm::ConstantInt::get(destTy, read_buf(buf + offset, size));
  }
  std::cout << "label: " << *rel->label << std::endl;
  char **label = rel->label;
    char *name = *label;
  GlobalVariable *global = TheModule->getGlobalVariable(name);
  Constant *value = global->getInitializer();
  return llvm::ConstantExpr::getPtrToInt(global, destTy);
}

static Constant *buffer2Constants(Type *varType, CType *ctype, CRelocation *rel, char *buf, int offset) {
  Constant *constant;
  int size = ctype->size;
  switch(ctype->kind) {
    case CType::TY_CHAR:
    case CType::TY_SHORT:
    case CType::TY_INT:
    case CType::TY_LONG:
      constant = EmitIntegerInitialication(varType, ctype, buf, offset, rel);
      break;
    case CType::TY_ARRAY:
      constant = EmitArrayInitialization(static_cast<llvm::ArrayType *>(varType), ctype, buf, offset, rel);
      break;
    case CType::TY_UNION:
      constant = EmitUnionInitialization(static_cast<llvm::StructType *>(varType), ctype, buf, offset);
      break;
    case CType::TY_STRUCT:
      constant = EmitRecordInitialization(static_cast<llvm::StructType *>(varType), ctype, buf, offset);
      break; 
    case CType::TY_PTR:
      constant = EmitPointerInitialization(static_cast<llvm::PointerType *>(varType), ctype, buf, offset, rel);
      break;
    default:
      constant = Builder->getInt32(-1024);
      break;
  }
  return constant;
}

static Constant *yuc2Constants(Type *Ty, Ast *gvarNode) {
  Constant *constant = buffer2Constants(Ty, gvarNode->type, gvarNode->rel, gvarNode->init_data, 0);
  return constant;
}

static GlobalValue::LinkageTypes yuc2LinkageType(Ast *yucNode) {
  GlobalValue::LinkageTypes ret = GlobalValue::LinkageTypes::ExternalLinkage;
  if (yucNode->is_static) {
    ret = GlobalValue::LinkageTypes::InternalLinkage;
  }

  return ret;
}


static void InitializeModule(const string &moduleName) {
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>(moduleName, *TheContext);
  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
  ModuleBuilder = std::make_unique<CodeGenModule>(*TheModule);
}


/**
 * https://llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 * https://www.llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 */ 
GlobalVariable *createGlobalVar(Ast *yucNode) {
    string name = yucNode->name;
    CType *ctype = yucNode->type;
    std::cout << "createGlobalVar kind:" << CType::ctypeKindString(ctype->kind);
    if (ctype->base) {
      std::cout << " base " << CType::ctypeKindString(ctype->base->kind);
    }
    if (ctype->origin) {
      std::cout << " origin " << CType::ctypeKindString(ctype->origin->kind);
    }
    std::cout << endl;
    std::cout << "DesiredType: ";
    Type *DesiredType = yuc2LLVMType(ctype);
    Constant *initializer = yuc2Constants(DesiredType, yucNode);
    std::cout << "initializer success" << std::endl;
    TheModule->getOrInsertGlobal(name, DesiredType);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    gVar->setAlignment(MaybeAlign(yucNode->align));
    if (!yucNode->is_static) {
      gVar->setDSOLocal(true);
    }
    
    gVar->setInitializer(initializer);
    gVar->setLinkage(yuc2LinkageType(yucNode));
    return gVar;
}

static void emit_data(Ast *ast) {
  if (!ast) {
    return;
  }
  emit_data(ast->next);
  std::cout << "\nemit_data name:" << ast->name << endl;
  createGlobalVar(ast);
}

static FunctionType * buildFunctionType(Ast *funcNode) {
  std::vector<Type *> types;
  CType *funcType = funcNode->type;
  for (CType *paramType = funcType->params; paramType; paramType = paramType->next) {
    Type *type = yuc2LLVMType(paramType);
    types.push_back(type);
  }

  Type *RetTy = yuc2LLVMType(funcType->return_ty);
  bool isVarArg = funcType->is_variadic;

  FunctionType *functionType = FunctionType::get(RetTy, types, isVarArg);
  return functionType;
}

static CNode *getFuntionEnd(CNode *block) {
  CNode *cur = block->body;
  while(cur) {
    if (cur->kind == CNode::CNodeKind::ND_RETURN) {
      return cur;
      break;
    }
    cur = cur->next;
  }
  cerr << "no ret" << endl;
  return NULL;
}

Function *createFunc(Ast *funcNode) {
  FunctionType *funcType = buildFunctionType(funcNode);
  std::string funcName = funcNode->name;
  GlobalValue::LinkageTypes linkageType = yuc2LinkageType(funcNode);
  Function *fooFunc = Function::Create(funcType, linkageType, funcName, TheModule.get());
  if(!funcNode->is_static) {
    fooFunc->setDSOLocal(true);
  }
  return fooFunc;
}

void setFuncArgs(Function *Func, std::vector<std::string> FuncArgs) {
  unsigned Idx = 0;
  Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, ++Idx) {
      AI->setName(FuncArgs[Idx]);
    }
}

static Value *getValue(CNode *node, Type *returnType) {
  cout<< "getValue kind:" << CNode::node_kind_info(node->kind) << endl;
  switch(node->kind) {
    case CNode::CNodeKind::ND_NUM:
      return Builder->getInt32(node->val);
      break;
  }
  return NULL;
}

static void StartFunction(Ast *funcNode) {
  cout << "StartFunction " << endl;
}

static void FinishFunction(Function *Func, Ast *funcNode) {
  cout << "FinishFunction " << endl;
  CNode *end = getFuntionEnd(funcNode->body);
  CNode *ndCast = end->lhs;
  Type *returnType = Func->getReturnType();
  CNode *ndReturn = ndCast->lhs;
  Value *val = getValue(ndReturn, returnType);
  Builder->CreateRet(val);
}

static void prepareLocals(Function *Func, Ast *funcNode) {
  cout << "prepareLocals start" << endl;
  // reverse locals
  vector <Ast *> locals;
  for (Ast *local = funcNode->locals; local; local = local->next) {
    locals.push_back(local);
  }
  reverse(locals.begin(), locals.end());
  // allocal local variables and args
  for (vector<Ast *>::iterator iter = locals.begin(); iter != locals.end(); iter++) {
    Ast *local = (*iter);
    Type *localType = yuc2LLVMType(local->type);
    Value *localAddr = Builder->CreateAlloca(localType, nullptr);
    LocallAddress[local->offset] = localAddr;
  }

  // reverse args
  vector<Ast *> args;
  for (Ast *param = funcNode->params; param; param = param->next) {
    args.push_back(param);
  }
  reverse(args.begin(), args.end());

  // store args
  vector<Ast *>::iterator arg_iter = args.begin();
  Function::arg_iterator AI, AE;
  for(AI = Func->arg_begin(), AE = Func->arg_end(); AI != AE; ++AI, arg_iter++) {
    Value *argAddr = LocallAddress[(*arg_iter)->offset];
    Builder->CreateStore(AI, argAddr);
  }
}

static void buildFunctionBody(Function *Func, Ast *funcNode) {
  BasicBlock *entry = BasicBlock::Create(*TheContext, "", Func);
  Builder->SetInsertPoint(entry);

  prepareLocals(Func, funcNode);
}

void buildFunction(Ast *funcNode) {
  cout << "buildFunction " << funcNode->name << endl;
  Function *fooFunc = createFunc(funcNode);
  StartFunction(funcNode);
  buildFunctionBody(fooFunc, funcNode);
  FinishFunction(fooFunc, funcNode);
  verifyFunction(*fooFunc);
}

static void emit_text(Ast *ast) {
  Ast *fn = ast;
  if (!fn ||!fn->is_function || !fn->is_definition) {
    return;
  }
  buildFunction(fn);
}

void processAnnonVar(Ast *ast) {
  if (ast) {
    processAnnonVar(ast->next);
    string name = ast->name;
    std::cout << "cur name:" << name << endl;
    const std::string &Str = ast->init_data;
    annonGlobalVar[name] = CGM().GetAddrOfConstantCString(Str, nullptr);
  }
}

void yuc::ir_gen(Ast *ast, std::ofstream &out, const string &moduleName) {
  cout << "ir_gen start\n";
  if (!ast) {
    std::cerr << "no ast" << std::endl;
    return;
  }

  Ast *annonP, *namedP;
  Ast **annon = &annonP, **named =&namedP;
  for (Ast *cur = ast; cur; cur = cur->next) {
    string name = cur->name;
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

  InitializeModule(moduleName);
  processAnnonVar(annonP);
  emit_data(namedP);
  // emit_text(ast);

  TheModule->print(errs(), nullptr);
  std::cout << "yuc end" << std::endl;
}

