#include "chibicc.h"
using namespace yuc;

int main(int argc, char *argv[]) {
	ofstream out_file("output_file.txt");
	Obj prog;

	ir_gen(prog, out_file);
	return 0;
}