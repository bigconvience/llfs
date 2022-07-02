#include "chibi2yuc.h"
#include "yuc.h"

using namespace yuc;
using namespace std;

static CType *build_ctype(Type *ty);
static Ast *build_ast(Obj *obj);
static CMember *build_cmember(Member *member);
static CNode *gen_cnode(Node *node, CNode **ppCur);

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

static CMember *build_cmember(Member *member) {
  if (!member) {
    return NULL;
  }
  CMember *cur;
  cur = new CMember();
  cur->ty = build_ctype(member->ty);
  cur->idx = member->idx;
  cur->align = member->align;
  cur->offset = member->offset;

  cur->is_bitfield = member->is_bitfield;
  cur->bit_offset = member->bit_offset;
  cur->bit_width = member->bit_offset;

  return cur;
}

static CType *build_ctype(Type *ty) {
  if (!ty) {
    return NULL;
  }
  CType *ctype = new CType();
  ctype->size = ty->size;
  ctype->align = ty->align;
  ctype->is_unsigned = ty->is_unsigned;
  ctype->is_atomic = ty->is_atomic;
  ctype->kind = static_cast<CType::CTypeKind>(ty->kind);
  ctype->origin = build_ctype(ty->origin);
  ctype->base = build_ctype(ty->base);
  ctype->array_len = ty->array_len;

  gen_cnode(ty->vla_len, &ctype->vla_len);
  ctype->vla_size = build_ast(ty->vla_size);

  int memberCount = 0;
  CMember **ppMem = &ctype->members;
  for (Member *member = ty->members; member; member = member->next) {
    memberCount++;
    *ppMem = build_cmember(member);
    ppMem = &(*ppMem)->next;
  }
  ctype->memberCount = memberCount;
  ctype->is_flexible = ty->is_flexible;
  ctype->is_packed = ty->is_packed;

  ctype->return_ty = build_ctype(ty->return_ty);
  ctype->params = build_ctype(ty->params);
  ctype->is_variadic = ty->is_variadic;
  ctype->next = build_ctype(ty->next);

  return ctype;
}

static Ast *build_ast(Obj *obj) {
  if (!obj) {
    return NULL;
  }
  Ast *cur = new Ast;
  cur->name = obj->name;
  cout << "build_ast, name:" << cur->name << endl;
  cur->next = build_ast(obj->next);
  cur->is_static = obj->is_static;
  cur->is_function = obj->is_function;
  cur->is_definition = obj->is_definition;
  cur->is_live = obj->is_live;
  cur->is_tentative = obj->is_tentative;
  cur->init_data = obj->init_data;
  cur->is_local = obj->is_local;
  cur->is_tls = obj->is_tls;
  cur->is_definition = obj->is_definition;
  cur->offset = obj->offset;
  cur->type = build_ctype(obj->ty);
  cur->locals = build_ast(obj->locals);
  cur->params = build_ast(obj->params);
  return cur;
}

static Ast **emit_data(Obj *prog, Ast **ppCur) {
  cout << "emit_data start" << endl;
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;
    Ast *cur = build_ast(var);
    cout << "global var name: " << var->name << endl;

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    cur->align = align;
    cur->type = build_ctype(var->ty);
    *ppCur = cur;
    ppCur = &cur->next;
   }
   cout << "emit_data end" << endl;
   return ppCur;
}


static CNode *gen_cnode(Node *node, CNode **ppCur) {
  if (!node) {
    *ppCur = NULL;
    return NULL;
  }
  int cur_count = ++stmt_count;
  int level = ++stmt_level;
  CNode *curNode, *tmpCNode;
  curNode = new CNode;
  curNode->kind = static_cast<CNode::CNodeKind>(node->kind);
  curNode->type = build_ctype(node->ty);
  *ppCur = curNode;

  output << buildSeperator(level, "gen_cnode ==> ") << cur_count
    << " kind:" << curNode->kind 
    << " "<< CNode::node_kind_info(curNode->kind) << endl;

  gen_cnode(node->lhs, &curNode->lhs);
  gen_cnode(node->rhs, &curNode->rhs);

  // ND_IF
  gen_cnode(node->cond, &curNode->cond);
  gen_cnode(node->then, &curNode->then);
  gen_cnode(node->els, &curNode->els);

  // ND_FOR
  gen_cnode(node->init, &curNode->init);
  gen_cnode(node->inc, &curNode->inc);

  if (node->brk_label) {
    curNode->brk_label = node->brk_label;
  }
  if (node->cont_label) {
    curNode->cont_label = node->cont_label;
  }
  
  // ND_BLOCK
  ppCur = &curNode->body;
  for (Node *n = node->body; n; n = n->next) {
    tmpCNode = gen_cnode(n, ppCur);
    ppCur = &tmpCNode->next;
  }

  // ND_SWITCH
  ppCur = &curNode->case_next;
  for (Node *n = node->case_next; n; n = n->case_next) {
    tmpCNode = gen_cnode(n, ppCur);
    ppCur = &tmpCNode->case_next;
  }
  gen_cnode(node->default_case, &curNode->default_case);

  // Case
  curNode->begin = node->begin;
  curNode->end = node->end;

  // ND_MEMBER
  curNode->member = build_cmember(node->member);

  // Function call
  curNode->func_ty = build_ctype(node->func_ty);
  ppCur = &curNode->args;
  for (Node *arg = node->args; arg; arg = arg->next) {
    tmpCNode = gen_cnode(arg, ppCur);
    ppCur = &tmpCNode->next;
  }
  curNode->pass_by_stack = node->pass_by_stack;
  curNode->ret_buffer = build_ast(node->ret_buffer);

  // ND_GOTO
  if (node->label) {
    curNode->label = node->label;
  }
  if (node->unique_label) {
    curNode->unique_label = node->unique_label;
  }
  gen_cnode(node->goto_next, &curNode->goto_next); 

  // ND_ASM
  if (node->asm_str) {
    curNode->asm_str = node->asm_str;
  }

  // Atomic compare-and-swap
  gen_cnode(node->cas_addr, &curNode->cas_addr);
  gen_cnode(node->cas_old, &curNode->cas_old);
  gen_cnode(node->cas_new, &curNode->cas_new);

  // Atomic op= operators
  curNode->atomic_addr = build_ast(node->atomic_addr);
  gen_cnode(node->atomic_expr, &curNode->atomic_expr);

  // Variable
  curNode->var = build_ast(node->var);

  // Numeric literal
  curNode->fval = node->fval;
  curNode->val = node->val;

  output << buildSeperator(level, "gen_cnode end <<<=== ") <<  cur_count << endl;
  --stmt_level;
  return curNode;
}


static Ast **emit_text(Obj *prog, Ast **ppCur) {
  output << "emit_text start" << endl;
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;
    if (!fn->is_live)
      continue;
    output << "\nemit_text, fn name:"<< fn->name << "\n"; 
    Ast *cur = build_ast(fn);

    stmt_count = 0;
    stmt_level = 0;
    gen_cnode(fn->body, &cur->body);
    
    *ppCur = cur;
    ppCur = &cur->next;
  }
  output << "emit_text end" << endl;
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