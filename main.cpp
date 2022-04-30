#include "chibicc.h"
using namespace std;

void test_createGlobalVar() {
	FILE *out_file= fopen("data.out", "w");
	Obj *prog = new Obj;
	prog->is_function = false;
	prog->is_definition = true;
	prog->is_static = false;
	prog->name = "globalVar";
	Type *type = new Type();
	prog->ty = type;
	type->size = 4;
	type->align = 4;
	type->kind = TY_INT;
	int number = 21;
	char *init = new char[4];
	prog->init_data = init;
	init[0] = 21;

	codegen(prog, out_file);
}

bool opt_fcommon = true;

int main(int argc, char *argv[]) {
	test_createGlobalVar();
	return 0;
}