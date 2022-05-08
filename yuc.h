#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

namespace yuc {
	class CType {
	public:
		typedef enum {
			ArrayType = 0, TypedPointerType, FunctionType, IntegerType,
			PointerType, classType, VectorType
		} CTypeKind;
		CTypeKind kind;
		int size;
		bool is_unsigned;
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
	};

	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}

#endif