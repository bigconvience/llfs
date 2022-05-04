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
		enum LinkageTypes {
		  ExternalLinkage = 0, AvailableExternallyLinkage, LinkOnceAnyLinkage, LinkOnceODRLinkage,
		  WeakAnyLinkage, WeakODRLinkage, AppendingLinkage, InternalLinkage,
		  PrivateLinkage, ExternalWeakLinkage, CommonLinkage
		};
		bool is_function;
		bool is_definition;
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