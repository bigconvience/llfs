#include <stdio.h>

int s = 1;
int t = 2;

int sum(int, int);

int main() {
	int result = sum(s, t);
	return result;
}

int sum(int a, int b) {
	return a + b;
}