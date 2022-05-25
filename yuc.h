#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

namespace yuc {
typedef class CType CType;
typedef class CNode CNode;
typedef class CMember CMember;
typedef class CRelocation CRelocation;
typedef class CToken CToken;

	class CValue {
	public:
		int64_t val;
	};

	// AST node
	class Ast {
	public:
		Ast *next;
		string name;    // Variable name
		CType *type;      // Type
		CToken *tok;    // representative token
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
	  CRelocation *rel;
	  CValue *initializer;

	  // Function
	  bool is_inline;
	  Ast *params;
	  CNode *body;
	  Ast *locals;
	  Ast *va_area;
	  Ast *alloca_bottom;
	  int stack_size;
	  
	  // Static inline function
	  bool is_live;
	  bool is_root;
	};

	class CToken {
	public:
		// Token
		typedef enum {
		  TK_IDENT,   // Identifiers
		  TK_PUNCT,   // Punctuators
		  TK_KEYWORD, // Keywords
		  TK_STR,     // String literals
		  TK_NUM,     // Numeric literals
		  TK_PP_NUM,  // Preprocessing numbers
		  TK_EOF,     // End-of-file markers
		} CTokenKind;

		CTokenKind kind;
	  CToken *next;      // Next token
	  int64_t val;      // If kind is TK_NUM, its value
	  long double fval; // If kind is TK_NUM, its value
	  char *loc;        // Token location
	  int len;          // Token length
	  CType *ty;         // Used if TK_NUM or TK_STR
	  char *str;        // String literal contents including terminating '\0'

	  char *filename;   // Filename
	  int line_no;      // Line number
	  int line_delta;   // Line number
	  bool at_bol;      // True if this token is at beginning of line
	  bool has_space;   // True if this token follows a space character
	};

	class CType {
	public:
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
		  TY_PTR,
		  TY_FUNC,
		  TY_ARRAY,
		  TY_VLA, // variable-length array
		  TY_STRUCT,
		  TY_UNION,
		} CTypeKind;
		
		CTypeKind kind;
		int size;
		bool is_unsigned;
		bool is_atomic;
		CType *origin;
		CType *base;

		int array_len;
		CNode *vla_len;
		Ast *vla_size;

		bool is_flexible;
		bool is_packed;

		// Function type
		CType *return_ty;
		CType *params;
		bool is_variadic;
		CType *next;
	};

	class CNode {
	public:
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
		  ND_ASSIGN,    // =
		  ND_COND,      // ?:
		  ND_COMMA,     // ,
		  ND_MEMBER,    // . (struct member access)
		  ND_ADDR,      // unary &
		  ND_DEREF,     // unary *
		  ND_NOT,       // !
		  ND_BITNOT,    // ~
		  ND_LOGAND,    // &&
		  ND_LOGOR,     // ||
		  ND_RETURN,    // "return"
		  ND_IF,        // "if"
		  ND_FOR,       // "for" or "while"
		  ND_DO,        // "do"
		  ND_SWITCH,    // "switch"
		  ND_CASE,      // "case"
		  ND_BLOCK,     // { ... }
		  ND_GOTO,      // "goto"
		  ND_GOTO_EXPR, // "goto" labels-as-values
		  ND_LABEL,     // Labeled statement
		  ND_LABEL_VAL, // [GNU] Labels-as-values
		  ND_FUNCALL,   // Function call
		  ND_EXPR_STMT, // Expression statement
		  ND_STMT_EXPR, // Statement expression
		  ND_VAR,       // Variable
		  ND_VLA_PTR,   // VLA designator
		  ND_NUM,       // Integer
		  ND_CAST,      // Type cast
		  ND_MEMZERO,   // Zero-clear a stack variable
		  ND_ASM,       // "asm"
		  ND_CAS,       // Atomic compare-and-swap
		  ND_EXCH,
		} CNodeKind;

		CNodeKind kind;
		CNode *next;
		CType *type;

		CNode *lhs;
		CNode *rhs;
		CNode *body;
  	// Variable
  	Ast *var;
		// Numeric literal
	  int64_t val;
	  long double fval;
	};



	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}

#endif