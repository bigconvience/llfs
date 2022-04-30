#ifndef YUC_H
#define YUC_H

#include <string>
#include <fstream>
#include <iostream>
using namespace std;

namespace yuc {
	struct Ast {
		bool is_function;
		bool is_static;
		string name;
		int align;
		int initializer;
		Ast *next;
	};

	void ir_gen(Ast *ast, ofstream &out);
}

#endif