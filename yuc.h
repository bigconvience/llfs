#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
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
		int align;
		bool is_unsigned;
		bool is_atomic;
		CType *origin;
		CType *base;

		int array_len;
		CNode *vla_len;
		Ast *vla_size;

		CMember *members;
		int memberCount;
		bool is_flexible;
		bool is_packed;

		// Function type
		CType *return_ty;
		CType *params;
		bool is_variadic;
		CType *next;

		// Union field
  	CMember *union_field;

		static string ctypeKindString(CTypeKind kind) {
			string kindStr;
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
				  kindStr = "unknow type kind";
				  break;
				}
			return kindStr;
		}
	};

	class CNode {
	public:
		typedef enum {
		  ND_NULL_EXPR, //0 Do nothing
		  ND_ADD,       //1 +
		  ND_SUB,       //2 -
		  ND_MUL,       //3 *
		  ND_DIV,       //4 /
		  ND_NEG,       //5 unary -
		  ND_MOD,       //6 %
		  ND_BITAND,    //7 &
		  ND_BITOR,     //8 |
		  ND_BITXOR,    //9 ^
		  ND_SHL,       //10 <<
		  ND_SHR,       //11 >>
		  ND_EQ,        //12 ==
		  ND_NE,        //13 !=
		  ND_LT,        //14 <
		  ND_LE,        //15 <=
		  ND_ASSIGN,    //16 =
		  ND_COND,      //17 ?:
		  ND_COMMA,     //18 ,
		  ND_MEMBER,    //19 . (struct member access)
		  ND_ADDR,      //20 unary &
		  ND_DEREF,     //21 unary *
		  ND_NOT,       //22 !
		  ND_BITNOT,    //23 ~
		  ND_LOGAND,    //24 &&
		  ND_LOGOR,     //25 ||
		  ND_RETURN,    //26 "return"
		  ND_IF,        //27 "if"
		  ND_FOR,       //28 "for" or "while"
		  ND_DO,        //29 "do"
		  ND_SWITCH,    //30 "switch"
		  ND_CASE,      //31 "case"
		  ND_BLOCK,     //32 { ... }
		  ND_GOTO,      //33 "goto"
		  ND_GOTO_EXPR, //34 "goto" labels-as-values
		  ND_LABEL,     //35 Labeled statement
		  ND_LABEL_VAL, //36 [GNU] Labels-as-values
		  ND_FUNCALL,   //37 Function call
		  ND_EXPR_STMT, //38 Expression statement
		  ND_STMT_EXPR, //39 Statement expression
		  ND_VAR,       //40 Variable
		  ND_VLA_PTR,   //41 VLA designator
		  ND_NUM,       //42 Integer
		  ND_CAST,      //43 Type cast
		  ND_MEMZERO,   //44 Zero-clear a stack variable
		  ND_ASM,       //45 "asm"
		  ND_CAS,       //46 Atomic compare-and-swap
		  ND_EXCH,
		} CNodeKind;

		CNodeKind kind;
		CNode *next;
		CType *type;

		CNode *lhs;
		CNode *rhs;

		// "if" or "for" statement
	  CNode *cond;
	  CNode *then;
	  CNode *els;
	  CNode *init;
	  CNode *inc;

	  // "break" and "continue" labels
	  string brk_label;
	  string cont_label;

	  // Block or statement expression
  	CNode *body;

	  // Struct member access
	  CMember *member;

	  // Function call
	  CType *func_ty;
	  CNode *args;
	  bool pass_by_stack;
	  Ast *ret_buffer;

	  // Goto or labeled statement, or labels-as-values
	  string label;
	  string unique_label;
	  CNode *goto_next;

	  // Switch
	  CNode *case_next;
	  CNode *default_case;

	  // Case
	  long begin;
	  long end;

	  // "asm" string literal
	  string asm_str;

	  // Atomic compare-and-swap
	  CNode *cas_addr;
	  CNode *cas_old;
	  CNode *cas_new;

	  // Atomic op= operators
	  Ast *atomic_addr;
	  CNode *atomic_expr;

	  // Variable
	  Ast *var;

	  // Numeric literal
	  int64_t val;
	  long double fval;


		static string node_kind_info(CNodeKind kind) {
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
				
				default:
					return "unkonw";
			}
		}
	};

	// Struct member
	class CMember {
	public:
	  CMember *next;
	  CType *ty;
	  CToken *tok; // for error message
	  CToken *name;
	  int idx;
	  int align;
	  int offset;

	  // Bitfield
	  bool is_bitfield;
	  int bit_offset;
	  int bit_width;
	};

	class CRelocation {
	public:
		CRelocation *next;
	  int offset;
	  char **label;
	  long addend;
	};

	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}

#endif