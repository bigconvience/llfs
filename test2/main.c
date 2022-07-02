#include <stdio.h>

char *out = "hello world\n";

union Data {
	int x;
	char y[8];
} Test1;

int main() {
	printf("%s", out);
	printf("size %ld\n", sizeof(Test1));
	for (int i = 0; i < 8; i++) {
		Test1.y[i] = i + '0';
	}
	printf("%s\n", Test1.y);
	return 0;
}