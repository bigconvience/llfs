#include "chibi2yuc.h"
#include "yuc.h"

using namespace yuc;
using namespace std;

static void dump_ast(Ast *ast) {
  cout << "dump_ast\n\t";
  for (Ast *cur = ast; cur; cur = cur->next) {
    cout << " name: " << cur->name 
    << " is_function: " << cur->is_function
    << " is_definition: " << cur->is_definition 
    << "\n\t";
  }
  cout << endl;
}

static CType *build_ctype(Obj *obj) {
  CType *ctype = new CType();
  Type *ty = obj->ty;
  ctype->size = ty->size;
  ctype->is_unsigned = ty->is_unsigned;
  TypeKind kind = ty->kind;
  cout << " build ctype: kind " << kind << endl;
  switch(kind) {
    case TY_INT:
      ctype->kind = CType::IntegerType;
      break;
    case TY_ARRAY:
      ctype->kind = CType::ArrayType;
      break;
    default:
      cerr << "unkonw kind: " << kind << endl;
      break;
  }
  return ctype;
}

static CValue *build_cvalue(Obj *obj) {
  CValue *cvalue = new CValue();
  Type *ty = obj->ty;
  int val;
  switch(ty->kind) {
    case TY_INT:
      val = *(int *)obj->init_data;
      cvalue->val = val;
      break;
  }

  return cvalue;
}

static Ast *buildAst(Obj *obj) {
  Ast *cur = new Ast;
  if (!obj) {
    return cur;
  }
  cur->name = obj->name;
  cur->is_static = obj->is_static;
  cur->is_function = obj->is_function;
  cur->is_definition = obj->is_definition;
  cur->is_live = obj->is_live;
  return cur;
}

static Ast **emit_data(Obj *prog, Ast **ppCur) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;
    Ast *cur = buildAst(var);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    cur->align = align;
    cur->type = build_ctype(var);
    cur->initializer = build_cvalue(var);

    *ppCur = cur;
    ppCur = &cur->next;
   }
   return ppCur;
}

static Ast **emit_text(Obj *prog, Ast **ppCur) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    if (!fn->is_live)
      continue;
    cout << "emit_text, fn name:"<< fn->name << "\n"; 
    Ast *cur = buildAst(fn);
    dump_ast(cur);

    *ppCur = cur;
    ppCur = &cur->next;
  }
  return ppCur;
}

void codegen_yuc(Obj *prog, const string file_name) {
  ofstream out_put("./test2/ir_output.out");
  Ast *pHead;
  Ast **last;
  last = emit_data(prog, &pHead);
  last = emit_text(prog, last);
  dump_ast(pHead);
  ir_gen(pHead, out_put, file_name);
}