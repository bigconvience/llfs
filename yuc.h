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

	struct Ast {
		bool is_function;
		bool is_static;
		string name;
		int align;
		int initializer;
		Ast *next;
		LinkageTypes linkage_type;
		bool is_preemptable;
	};

	void ir_gen(Ast *ast, ofstream &out);
}

#endif