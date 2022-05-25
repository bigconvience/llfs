#include "chibi2yuc.h"
#include "yuc.h"

using namespace yuc;
using namespace std;

static CNode *gen_stmt(Node *node, CNode **ppCur);
static CNode *gen_expr(Node *node, CNode **ppCur);
static CNode *gen_addr(Node *node, CNode **ppCur);
static CType *build_ctype(Type *ty);
static CNode *new_cnode(Node *node);
static Ast *buildAst(Obj *obj);

static int stmt_count = 0;
static int stmt_level = 0;
static std::ofstream output("./test2/ast_origin.out");

static string buildSeperator(int count, string target) {
  string result = "";
  for (int i = 1; i < count; i++) {
    result.append("\t");
  }
  result.append(target);
  return result;
}

static CNode *gen_addr(Node *node, CNode **ppCur) {
  CNode *curNode = new_cnode(node);
  *ppCur = curNode;
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "gen_addr")
      << " node kind:" << node->kind << endl;
  cur_level++;
  curNode->type = build_ctype(node->ty);
  switch(node->kind) {
    case ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
  }
  --stmt_level;
  return curNode;
}

static void cast(Type *from, Type *to) {
  int cur_level = ++stmt_level;
  output << buildSeperator(cur_level, "cast")
      << " from:" << from->kind << " to:" << to->kind << endl; 
  --stmt_level;
}

static CType *build_ctype(Type *ty) {
  if (!ty) {
    return NULL;
  }
  CType *ctype = new CType();
  ctype->size = ty->size;
  ctype->is_unsigned = ty->is_unsigned;
  TypeKind kind = ty->kind;
  cout << "build ctype size " << ty->size
      << " kind " << kind << ": ";
  ctype->kind = static_cast<CType::CTypeKind>(ty->kind);
  switch(kind) {
    case TY_STRUCT:
      cout << "TY_STRUCT";
      break;
      case TY_VOID:
      cout << "TY_VOID";
      break;
    case TY_CHAR:
      cout << "TY_CHAR";
      break;
    case TY_INT:
      cout << "TY_INT";
      break;
    case TY_ARRAY:
      cout << "TY_ARRAY";
      break;
    case TY_PTR:
      cout << "TY_PTR, pointer base \n";
      ctype->base = build_ctype(ty->base);
      break;
    default:
      cerr << "unknow type kind: " << kind << endl;
      break;
  }
  cout << endl;
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

static CNode *new_cnode(Node *node) {
  CNode *cNode = new CNode;
  cNode->kind = static_cast<CNode::CNodeKind>(node->kind);
  cNode->var = buildAst(node->var);
  return cNode;
}

static Ast *buildAst(Obj *obj) {
  if (!obj) {
    return NULL;
  }
  Ast *cur = new Ast;
  cur->name = obj->name;
  cur->is_static = obj->is_static;
  cur->is_function = obj->is_function;
  cur->is_definition = obj->is_definition;
  cur->is_live = obj->is_live;
  cur->is_tentative = obj->is_tentative;
  cur->is_local = obj->is_local;
  cur->is_tls = obj->is_tls;
  cur->is_definition = obj->is_definition;
  return cur;
}

static Ast **emit_data(Obj *prog, Ast **ppCur) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;
    Ast *cur = buildAst(var);
    cout << "global var name: " << var->name << endl;

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    cur->align = align;
    cur->type = build_ctype(var->ty);
    cur->initializer = build_cvalue(var);
    cur->init_data = var->init_data;
    cur->is_tls = var->is_tls;

    *ppCur = cur;
    ppCur = &cur->next;
   }
   return ppCur;
}

static CNode *gen_expr(Node *node, CNode **ppCur) {
  int cur_level = ++stmt_level;
  CNode *curNode = new_cnode(node);
  *ppCur = curNode;
  output << buildSeperator(cur_level, "gen_expr start, kind:") 
  << node->kind <<  endl; 
  cur_level++;
  switch(node->kind) {
    case ND_NULL_EXPR:
      output << buildSeperator(cur_level, "ND_NULL_EXPR:") << node->kind << endl;
      break;
    case ND_COMMA:
      output << buildSeperator(cur_level, "ND_COMMA:") << node->kind << endl;
      gen_expr(node->lhs, &(*ppCur)->lhs);
      gen_expr(node->rhs, &(*ppCur)->rhs);
      break;
    case ND_COND:
      output << buildSeperator(cur_level, "ND_COND:") << node->kind << endl;
      break;
    case ND_ASSIGN:
      output << buildSeperator(cur_level, "ND_ASSIGN:") << node->kind << endl;
      gen_addr(node->lhs, &(*ppCur)->lhs);
      gen_expr(node->rhs, &(*ppCur)->rhs);
      if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {

      }
      break;
    case ND_NUM:
      output << buildSeperator(cur_level, "ND_NUM:") << node->kind << endl;
      break;
    case ND_MEMZERO:
      output << buildSeperator(cur_level, "ND_MEMZERO:") << node->kind << endl;
      break;
    case ND_VAR:
      output << buildSeperator(cur_level, "ND_VAR:") << node->kind << endl;
      break;
    case ND_CAST:
      output << buildSeperator(cur_level, "ND_CAST:") << node->kind << endl;
      gen_expr(node->lhs, ppCur);
      cast(node->lhs->ty, node->ty);
      break;
    case ND_ADD:
      output << buildSeperator(cur_level, "ND_ADD:") << node->kind << endl;
      break;

  }
  stmt_level--;
  cur_level--;
  output << buildSeperator(cur_level, "gen_expr end") << endl; 
  return curNode;
}

static CNode *gen_stmt(Node *node, CNode **ppCur) {
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  CNode *curNode = new_cnode(node);
  *ppCur = curNode;
  output << buildSeperator(level, "gen_stmt start ==> ") << cur_count << endl;
  switch(node->kind) {
    case ND_BLOCK: // 32
      output << buildSeperator(level+1, "ND_BLOCK:") << ND_BLOCK << "\n"; 
      ppCur = &(*ppCur)->body;
      for (Node *n = node->body; n; n = n->next) {
        CNode *temp = gen_stmt(n, ppCur);
        ppCur = &temp->next;
      }
      break;
    case ND_RETURN: // 26
      output << buildSeperator(level+1, "ND_RETURN:") << ND_RETURN << "\n"; 
      if (node->lhs) {
        Type *ty = node->lhs->ty;
        curNode->type = build_ctype(ty);
        ppCur = &(*ppCur)->next;
        gen_expr(node->lhs, ppCur);
      }
      break;
    case ND_EXPR_STMT: // 38
      output << buildSeperator(level+1, "ND_EXPR_STMT:")<< ND_EXPR_STMT << endl; 
      gen_expr(node->lhs, &(*ppCur)->lhs);
      break;
    default:
      output << buildSeperator(level+1, "gen_stmt unknow kind: ") << node->kind << endl;
      break;
  }
  output << buildSeperator(level, "gen_stmt end <<<=== ") <<  cur_count << endl;
  --stmt_level;
  return curNode;
}

static Ast **emit_text(Obj *prog, Ast **ppCur) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    if (!fn->is_live)
      continue;
    output << "\nemit_text, fn name:"<< fn->name << "\n"; 
    Ast *cur = buildAst(fn);
    Ast **pParam = &cur->params;

    for (Obj *var = fn->params; var; var = var->next) {

      Ast *param = new Ast();
      param->name = var->name;
      param->type = build_ctype(var->ty);

      *pParam = param;
      pParam = &param->next; 
    }
    stmt_count = 0;
    gen_stmt(fn->body, &cur->body);
    
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
  ir_gen(pHead, out_put, file_name);
}