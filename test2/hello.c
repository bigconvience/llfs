#include <stdio.h>

char g17[] = "foobar";
char *g21 = &g17+3;
char *g22 = &g17-3;
char *g23 = g17+3;
char *g24 = g17-3;

int main() {
	printf("g21:%ld\n", g21 - g17);
	printf("g22:%ld\n", g22 - g17);
	printf("g23:%ld\n", g23 - g17);
	printf("g24:%ld\n", g24 - g17);
	return 0;
}