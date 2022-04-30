#include <stdio.h>

int variable = 21;
static int stac_val = 22;
int main(){
	printf("hello: %d %d\n", variable, stac_val);
	return 0;
}