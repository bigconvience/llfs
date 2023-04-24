#ifndef CHIBICC_H
#define CHIBICC_H

#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <glob.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <string>

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#ifndef __GNUC__
# define __attribute__(x)
#endif

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;
typedef struct Hideset Hideset;
typedef struct Initializer Initializer;

//
// strings.c
//

typedef struct {
  char **data;
  int capacity;
  int len;
} StringArray;

void strarray_push(StringArray *arr, const char *s);
char *format(char *fmt, ...) __attribute__((format(printf, 1, 2)));

//
// tokenize.c
//

// Token
typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Punctuators
  TK_KEYWORD, // Keywords
  TK_STR,     // String literals
  TK_NUM,     // Numeric literals
  TK_PP_NUM,  // Preprocessing numbers
  TK_EOF,     // End-of-file markers
} TokenKind;

typedef struct {
  char *name;
  int file_no;
  char *contents;

  // For #line directive
  char *display_name;
  int line_delta;
} File;

// Token type
typedef struct Token Token;
struct Token {
  TokenKind kind;   // Token kind
  Token *next;      // Next token
  int64_t val;      // If kind is TK_NUM, its value
  long double fval; // If kind is TK_NUM, its value
  char *loc;        // Token location
  int len;          // Token length
  Type *ty;         // Used if TK_NUM or TK_STR
  char *str;        // String literal contents including terminating '\0'

  File *file;       // Source location
  char *filename;   // Filename
  int line_no;      // Line number
  int line_delta;   // Line number
  bool at_bol;      // True if this token is at beginning of line
  bool has_space;   // True if this token follows a space character
  Hideset *hideset; // For macro expansion
  Token *origin;    // If this is expanded from a macro, the original token
};

noreturn void error(char *fmt, ...) __attribute__((format(printf, 1, 2)));
noreturn void error_at(char *loc, char *fmt, ...) __attribute__((format(printf, 2, 3)));
noreturn void error_tok(Token *tok, char *fmt, ...) __attribute__((format(printf, 2, 3)));
void warn_tok(Token *tok, char *fmt, ...) __attribute__((format(printf, 2, 3)));
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
void convert_pp_tokens(Token *tok);
File **get_input_files(void);
File *new_file(char *name, int file_no, char *contents);
Token *tokenize_string_literal(Token *tok, Type *basety);
Token *tokenize(File *file);
Token *tokenize_file(char *filename);

#define unreachable() \
  error("internal error at %s:%d", __FILE__, __LINE__)

//
// preprocess.c
//

char *search_include_paths(char *filename);
void init_macros(void);
void define_macro(char *name, char *buf);
void undef_macro(char *name);
Token *preprocess(Token *tok);

//
// parse.c
//

// Variable or function
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;    // Variable name
  Type *ty;      // Type
  Token *tok;    // representative token
  bool is_local; // local or global/function
  int align;     // alignment

  // Local variable
  int offset;

  // Global variable or function
  bool is_function;
  bool is_definition;
  bool is_static;

  // Global variable
  bool is_tentative;
  bool is_tls;
  char *init_data;
  Relocation *rel;

  // Function
  bool is_inline;
  Obj *params;
  Node *body;
  Obj *locals;
  Obj *va_area;
  Obj *alloca_bottom;
  int stack_size;
  int retCount;

  // Static inline function
  bool is_live;
  bool is_root;
  StringArray refs;

  // constant data;
  bool is_constant_var;
  // Local variable init
  Initializer *init;
  // scope_index
  int scope_level;
};

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
typedef struct Relocation Relocation;
struct Relocation {
  Relocation *next;
  int offset;
  char **label;
  long addend;
};

// AST node
typedef enum {
  ND_NULL_EXPR, // Do nothing
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_NEG,       // unary -
  ND_MOD,       // %
  ND_BITAND,    // &
  ND_BITOR,     // |
  ND_BITXOR,    // ^
  ND_SHL,       // <<
  ND_SHR,       // >>
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_GT,        // >
  ND_GE,        // >=
  ND_ASSIGN,    // = 16
  ND_COND,      // ?:
  ND_COMMA,     // ,
  ND_MEMBER,    // . (struct member access)
  ND_ADDR,      // unary & 20
  ND_DEREF,     // unary *
  ND_NOT,       // !
  ND_BITNOT,    // ~
  ND_LOGAND,    // &&
  ND_LOGOR,     // ||
  ND_RETURN,    // "return"
  ND_IF,        // "if"
  ND_FOR,       // "for" or "while"
  ND_DO,        // "do"
  ND_SWITCH,    // "switch" 30
  ND_CASE,      // "case"
  ND_BLOCK,     // { ... }
  ND_BLOCK_ITEM, // declaration or statement
  ND_GOTO,      // "goto"
  ND_GOTO_EXPR, // "goto" labels-as-values
  ND_LABEL,     // Labeled statement
  ND_LABEL_VAL, // [GNU] Labels-as-values
  ND_FUNCALL,   // Function call
  ND_EXPR_STMT, // Expression statement
  ND_STMT_EXPR, // Statement expression
  ND_VAR,       // Variable 40
  ND_VLA_PTR,   // VLA designator
  ND_NUM,       // Integer
  ND_CAST,      // Type cast
  ND_MEMZERO,   // Zero-clear a stack variable
  ND_ASM,       // "asm"
  ND_CAS,       // Atomic compare-and-swap
  ND_EXCH,      // Atomic exchange
  ND_POST_INC, // post++
  ND_POST_DEC, // post--
  ND_PREFIX_INC, // ++prefix
  ND_PREFIX_DEC, // --prefix
  ND_SUBSCRIPT, // postfix arr[idx]
  ND_DECL_VLA, // declare a vla
} NodeKind;

// operand kind
typedef enum {
  NUM_NUM, // num (+|-) num
  VLA_NUM, // VLA (+|-) num
  PTR_NUM, // ptr (+|-) num
  PTR_PTR, // ptr - ptr
} OperandKind;

// AST node type
struct Node {
  NodeKind kind; // Node kind
  OperandKind o_kind; // Operand kind
  Node *next;    // Next node
  Type *ty;      // Type, e.g. int or pointer to int
  Token *tok;    // Representative token

  Node *lhs;     // Left-hand side
  Node *rhs;     // Right-hand side

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // evaluated as int_1
  bool eval_as_bool;

  // "break" and "continue" labels
  char *brk_label;
  char *cont_label;

  // Block or statement expression
  Node *body;

  // Struct member access
  Member *member;

  // Function call
  Type *func_ty;
  Node *args;
  bool pass_by_stack;
  Obj *ret_buffer;

  // Goto or labeled statement, or labels-as-values
  char *label;
  char *unique_label;
  Node *goto_next;

  // Switch
  Node *case_next;
  Node *default_case;

  // Case
  long begin;
  long end;

  // "asm" string literal
  char *asm_str;

  // Atomic compare-and-swap
  Node *cas_addr;
  Node *cas_old;
  Node *cas_new;

  // Atomic op= operators
  Obj *atomic_addr;
  Node *atomic_expr;

  // Variable
  Obj *var;

  // Numeric literal
  int64_t val;
  long double fval;
  
  // stmt expr;
  Node *last_expr;
  Obj *last_var;

  // ptr + offset
  bool is_offset;
};

Node *new_cast(Node *expr, Type *ty);
int64_t const_expr(Token **rest, Token *tok);
Obj *parse(Token *tok);

//
// type.c
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_FLOAT,
  TY_DOUBLE,
  TY_LDOUBLE,
  TY_ENUM,
  TY_PTR, // index 10
  TY_FUNC,
  TY_ARRAY, // idnex 12
  TY_VLA, // variable-length array
  TY_STRUCT,
  TY_UNION,
  TY_DUMMY,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;           // sizeof() value
  int align;          // alignment
  bool is_unsigned;   // unsigned or signed
  bool is_atomic;     // true if _Atomic
  bool is_const;      // true if const-qualified
  Type *origin;       // for type compatibility check

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type *base;

  // Declaration
  Token *name;
  Token *name_pos;

  // Array
  int array_len;

  // Variable-length array
  Node *vla_len; // # of elements
  Obj *vla_size; // sizeof() value

  // Struct
  Member *members;
  Member *grouped_members;
  bool is_flexible;
  bool is_packed;
  bool has_bitfield;

  // Function type
  Type *return_ty;
  Type *params;
  bool is_variadic;
  Type *next;

  // Union field
  Member *union_field;

  bool is_typedef;
  Token *tag;
  int scope_level;
};

// Struct member
struct Member {
  Member *next;
  Type *ty;
  Token *tok; // for error message
  Token *name;
  int idx;
  int align;
  int offset;
  int type_idx; // index correspond

  // Bitfield
  bool is_bitfield;
  int bit_offset;
  int bit_width;
  int lhs_bits; // left shift bits
  int rhs_bits; // right shift bits
  int real_type_size; // grouped type size

  Member *grouped_member;
};

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
struct Initializer {
  Initializer *next;
  Type *ty;
  Token *tok;
  bool is_flexible;

  // If it's not an aggregate type and has an initializer,
  // `expr` has an initialization expression.
  Node *expr;

  // If it's an initializer for an aggregate type (e.g. array or struct),
  // `children` has initializers for its children.
  Initializer **children;

  // Only one member can be initialized for a union.
  // `mem` is used to clarify which member is initialized.
  Member *mem;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_uint;
extern Type *ty_ulong;

extern Type *ty_float;
extern Type *ty_double;
extern Type *ty_ldouble;

bool is_integer(Type *ty);
bool is_flonum(Type *ty);
bool is_numeric(Type *ty);
bool is_compatible(Type *t1, Type *t2);
bool is_struct(Type *ty);
bool is_record_type(Type *ty);
bool is_compound_type(Type *ty);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
Type *vla_of(Type *base, Node *expr);
Type *enum_type(void);
Type *struct_type(void);
Type *get_int_type(int size);
void add_type(Node *node);
bool is_const_expr(Node *node);
bool is_const_initializer(Initializer *init);

//
// codegen.c
//

void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);

//
// unicode.c
//

int encode_utf8(char *buf, uint32_t c);
uint32_t decode_utf8(char **new_pos, char *p);
bool is_ident1(uint32_t c);
bool is_ident2(uint32_t c);
int display_width(char *p, int len);

//
// hashmap.c
//

typedef struct {
  char *key;
  int keylen;
  void *val;
} HashEntry;

typedef struct {
  HashEntry *buckets;
  int capacity;
  int used;
} HashMap;

void *hashmap_get(HashMap *map, char *key);
void *hashmap_get2(HashMap *map, char *key, int keylen);
void hashmap_put(HashMap *map, char *key, void *val);
void hashmap_put2(HashMap *map, char *key, int keylen, void *val);
void hashmap_delete(HashMap *map, char *key);
void hashmap_delete2(HashMap *map, char *key, int keylen);
void hashmap_test(void);

//
// main.c
//

bool file_exists(char *path);

extern StringArray include_paths;
extern bool opt_fpic;
extern bool opt_fcommon;
extern char *base_file;


// generate llvm ir
void gen_ir(Obj *prog, const std::string &file_name);

static std::string ctypeKindString(TypeKind kind) {
  std::string kindStr;
  switch(kind) {
    case TY_VOID:
      kindStr = "TY_VOID";
      break;
    case TY_BOOL:
      kindStr = "TY_BOOL";
      break;
    case TY_CHAR:
      kindStr = "TY_CHAR";
      break;
    case TY_SHORT:
      kindStr = "TY_SHORT";
      break;
    case TY_INT:
      kindStr = "TY_INT";
      break;
    case TY_LONG:
      kindStr = "TY_LONG";
      break;
    case TY_FLOAT:
      kindStr = "TY_FLOAT";
      break;
    case TY_DOUBLE:
      kindStr = "TY_DOUBLE";
      break;
    case TY_LDOUBLE:
      kindStr = "TY_LDOUBLE";
      break;
    case TY_ENUM:
      kindStr = "TY_ENUM";
      break;
    case TY_PTR:
      kindStr = "TY_PTR";
      break;
    case TY_FUNC:
      kindStr = "TY_FUNC";
      break;
    case TY_ARRAY:
      kindStr = "TY_ARRAY";
      break;
    case TY_VLA:
      kindStr = "TY_VLA";
      break;
    case TY_STRUCT:
      kindStr = "struct";
      break;
    case TY_UNION:
      kindStr = "union";
      break;
    default:
      kindStr = std::to_string(kind);
      break;
    }
  return kindStr;
}

static std::string node_kind_info(NodeKind kind) {
  switch(kind) {
    case ND_NULL_EXPR:
      return "ND_NULL_EXPR";
    case ND_ADD:
      return "ND_ADD";
    case ND_SUB:
      return "ND_SUB";
    case ND_MUL:
      return "ND_MUL";
    case ND_DIV:
      return "ND_DIV";
    case ND_NEG:
      return "ND_NEG";
    case ND_MOD:
      return "ND_MOD";
    case ND_BITAND:
      return "ND_BITAND";
    case ND_BITOR:
      return "ND_BITOR";
    case ND_BITXOR:
      return "ND_BITXOR";
    case ND_SHL:
      return "ND_SHL";
    case ND_SHR:
      return "ND_SHR";
    case ND_EQ:
      return "ND_EQ";
    case ND_NE:
      return "ND_NE";
    case ND_LT:
      return "ND_LT";
    case ND_LE:
      return "ND_LE";
    case ND_ASSIGN:
      return "ND_ASSIGN";
    case ND_COND:
      return "ND_COND";
    case ND_COMMA:
      return "ND_COMMA";
    case ND_MEMBER:
      return "ND_MEMBER";
    case ND_ADDR:
      return "ND_ADDR";
    case ND_DEREF:
      return "ND_DEREF";
    case ND_NOT:
      return "ND_NOT";
    case ND_BITNOT:
      return "ND_BITNOT";
    case ND_LOGAND:
      return "ND_LOGAND";
    case ND_LOGOR:
      return "ND_LOGOR";
    case ND_RETURN:
      return "ND_RETURN";
    case ND_IF:
      return "ND_IF";
    case ND_FOR:
      return "ND_FOR";
    case ND_DO:
      return "ND_DO";
    case ND_SWITCH:
      return "ND_SWITCH";
    case ND_CASE:
      return "ND_CASE";
    case ND_BLOCK:
      return "ND_BLOCK";
    case ND_GOTO:
      return "ND_GOTO";
    case ND_GOTO_EXPR:
      return "ND_GOTO_EXPR";
    case ND_LABEL:
      return "ND_LABEL";
    case ND_LABEL_VAL:
      return "ND_LABEL_VAL";
    case ND_FUNCALL:
      return "ND_FUNCALL";
    case ND_BLOCK_ITEM:
      return "ND_BLOCK_ITEM";
    case ND_EXPR_STMT:
      return "ND_EXPR_STMT";
    case ND_STMT_EXPR:
      return "ND_STMT_EXPR";
    case ND_VAR:
      return "ND_VAR";
    case ND_VLA_PTR:
      return "ND_VLA_PTR";
    case ND_NUM:
      return "ND_NUM";
    case ND_CAST:
      return "ND_CAST";
    case ND_MEMZERO:
      return "ND_MEMZERO";
    case ND_ASM:
      return "ND_ASM";
    case ND_CAS:
      return "ND_CAS";
    case ND_EXCH:
      return "ND_EXCH";
    case ND_POST_INC:
      return "ND_POST_INC";
    case ND_POST_DEC:
      return "ND_POST_DEC";
    case ND_PREFIX_INC:
      return "ND_PREFIX_INC";
    case ND_PREFIX_DEC:
      return "ND_PREFIX_DEC";
    case ND_SUBSCRIPT:
      return "ND_SUBSCRIPT";
    case ND_DECL_VLA:
      return "ND_DECL_VLA";
    default:
      return std::to_string(kind);
  }
}
#endif