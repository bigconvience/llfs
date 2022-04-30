#include "chibicc.h"
#include "yuc.h"

using namespace std;
using namespace yuc;

static Ast *global_ast;
static FILE *output_file;

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static void emit_data(Obj *prog) {
  cout << "emit_data start" << endl;
  Ast head;
  Ast *cur = &head;
  for (Obj *var = prog; var; var = var->next) {
    cout << "emit_data name: " << var->name << endl;
    if (var->is_function || !var->is_definition)
      continue;

    cur = cur->next = new Ast;
    cur->name = var->name;
    if (var->is_static)
      println("  .local %s", var->name);
    else
      println("  .globl %s", var->name);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;
    cur->align = align;
    // Common symbol
    if (opt_fcommon && var->is_tentative) {
      println("  .comm %s, %d, %d", var->name, var->ty->size, align);
      continue;
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls)
        println("  .section .tdata,\"awT\",@progbits");
      else
        println("  .data");

      println("  .type %s, @object", var->name);
      println("  .size %s, %d", var->name, var->ty->size);
      println("  .align %d", align);
      println("%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .quad %s%+ld", *rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("  .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }

    // .bss or .tbss
    if (var->is_tls)
      println("  .section .tbss,\"awT\",@nobits");
    else
      println("  .bss");

    println("  .align %d", align);
    println("%s:", var->name);
    println("  .zero %d", var->ty->size);
  }
  global_ast = head.next;
}


void codegen(Obj *prog, FILE *out)
{ 
  output_file = out;
  emit_data(prog);
  ofstream out_put("ir_output.out");
	ir_gen(global_ast, out_put);
}