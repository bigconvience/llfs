#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
using namespace std;

namespace yuc {
	enum LinkageTypes {
	  ExternalLinkage = 0, AvailableExternallyLinkage, LinkOnceAnyLinkage, LinkOnceODRLinkage,
	  WeakAnyLinkage, WeakODRLinkage, AppendingLinkage, InternalLinkage,
	  PrivateLinkage, ExternalWeakLinkage, CommonLinkage
	};

	typedef enum {
		ArrayType = 0, TypedPointerType, FunctionType, IntegerType,
		PointerType, StructType, VectorType
	} CTypeKind;

	struct CType {
		CTypeKind kind;
		int size;
		bool is_unsigned;
	};

	struct CValue {
		int64_t val;
	};

	struct Ast {
		bool is_function;
		bool is_static;
		string name;
		int align;
		Ast *next;
		LinkageTypes linkage_type;
		bool is_preemptable;

		CType *type;
		CValue *initializer;
	};

	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}

#endif