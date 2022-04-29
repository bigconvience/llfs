#include "chibicc.h"
using namespace yuc;

void test_createGlobalVar() {
	ofstream out_file("output_file.txt");
	Obj prog;
	prog.is_function = false;
	prog.is_definition = true;

	ir_gen(&prog, out_file);
}

int main(int argc, char *argv[]) {
	test_createGlobalVar();
	return 0;
}