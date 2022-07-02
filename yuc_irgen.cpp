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
static Constant *buffer2Constants(Type *Ty, CType *ctype, char *buf, int offset);

static LLVMContext &getLLVMContext() {
  return *TheContext;
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
  for (CMember *member = ctype->members; member; member = member->next) {
    Types.push_back(yuc2LLVMType(member->ty));
  }
  llvm::StructType *type = llvm::StructType::get(*TheContext, Types, true);
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

  llvm::StructType *Entry = llvm::StructType::create(getLLVMContext());
  addRecordTypeName(ctype, Entry, "");

  llvm::StructType *Ty = Entry;
  ComputeRecordLayout(ctype, Ty);
  return Ty;
}

static Type *yuc2LLVMType(CType *ctype) {
  cout << "yuc2LLVMType start: "
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

llvm::Constant *EmitArrayInitialization(llvm::ArrayType *Ty, CType *ArrayType, char *buf, int offset) {
  SmallVector<llvm::Constant *, 16> Elts;
  unsigned NumElements = ArrayType->array_len;
  Elts.reserve(NumElements);
  int sz = ArrayType->base->size;
  Type *CommonElementType = Ty->getElementType();
  CType *baseTy = ArrayType->base;
  for(int i = 0; i < NumElements; i++) {
    llvm::Constant *C = buffer2Constants(CommonElementType, baseTy, buf, offset + sz * i);
    Elts.push_back(C);
  }

  return llvm::ConstantArray::get(
        llvm::ArrayType::get(CommonElementType, NumElements), Elts);
 }
 
llvm::Constant *EmitRecordInitialization(llvm::StructType *Ty, CType *ctype, char *buf, int offset) {
  SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = ctype->memberCount;
  Elements.reserve(NumElements);
  for (CMember *member = ctype->members; member; member = member->next) {
    Type *Ty = yuc2LLVMType(member->ty);
    llvm::Constant *constantValue = buffer2Constants(Ty, member->ty, buf, offset + member->offset);
    Elements.push_back(constantValue);
  }

  return llvm::ConstantStruct::get(Ty, Elements);
}  

static Constant *buffer2Constants(Type *varType, CType *ctype, char *buf, int offset) {
  Constant *constant;
  int size = ctype->size;
  switch(ctype->kind) {
    case CType::TY_CHAR:
    case CType::TY_SHORT:
    case CType::TY_INT:
    case CType::TY_LONG:
      constant = llvm::ConstantInt::get(varType, read_buf(buf + offset, size));
      break;
    case CType::TY_ARRAY:
      constant = EmitArrayInitialization(static_cast<llvm::ArrayType *>(varType),ctype, buf, offset);
      break;
    case CType::TY_UNION:
    case CType::TY_STRUCT:
      constant = EmitRecordInitialization(static_cast<llvm::StructType *>(varType), ctype, buf, offset);
      break; 
    default:
      constant = Builder->getInt32(-1024);
      break;
  }
  return constant;
}

static Constant *yuc2Constants(Type *Ty, Ast *gvarNode) {
  Constant *constant = buffer2Constants(Ty, gvarNode->type, gvarNode->init_data, 0);
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
}


/**
 * https://llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 * https://www.llvm.org/doxygen/classllvm_1_1GlobalVariable.html
 */ 
GlobalVariable *createGlobalVar(Ast *yucNode) {
    string name = yucNode->name;
    CType *ctype = yucNode->type;
    std::cout << "createGlobalVar kind:" << ctype->kind << endl;
    Type *type = yuc2LLVMType(yucNode->type);
    TheModule->getOrInsertGlobal(name, type);
    GlobalVariable *gVar = TheModule->getNamedGlobal(name);
    gVar->setAlignment(MaybeAlign(yucNode->align));
    if (!yucNode->is_static) {
      gVar->setDSOLocal(true);
    }
    Constant *initializer = yuc2Constants(type, yucNode);
    gVar->setInitializer(initializer);

    gVar->setLinkage(yuc2LinkageType(yucNode));
    return gVar;
}

static void emit_data(Ast *ast) {
  if (!ast) {
    return;
  }
  emit_data(ast->next);
  std::cout << "emit_data name:" << ast->name << endl;
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

void yuc::ir_gen(Ast *ast, std::ofstream &out, const string &moduleName) {
  cout << "ir_gen start\n";
  if (!ast) {
    std::cerr << "no ast" << std::endl;
    return;
  }
  InitializeModule(moduleName);
  emit_data(ast);
  emit_text(ast);

  // Builder->CreateGlobalStringPtr(StringRef("Hello, world!\n"));
  TheModule->print(errs(), nullptr);
  std::cout << "yuc end" << std::endl;
}

