#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

namespace yuc {
	// AST node
	class CType {
	public:
		typedef enum {
			ArrayType = 0, TypedPointerType, FunctionType, IntegerType,
			PointerType, classType, VectorType
		} CTypeKind;
		
		CTypeKind kind;
		int size;
		bool is_unsigned;

		CType *base;
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
	};

	class CValue {
	public:
		int64_t val;
	};

	class Ast {
	public:
		bool is_function;
		bool is_definition;
		bool is_static;
		bool is_live;
		string name;
		int align;
		Ast *next;
		bool is_preemptable;

		CType *type;
		CValue *initializer;

	 	// Function
	  bool is_inline;
	  Ast *params;
	  CNode *body;
	  Ast *locals;
	  Ast *va_area;
	  Ast *alloca_bottom;
	  
	  int stack_size;
	};

	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}

#endif