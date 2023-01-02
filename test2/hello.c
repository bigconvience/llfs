#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
  ASSERT(-32, ({ _Alignas(32) char x, y; &x-&y; }));
  ASSERT(32, ({ _Alignas(32) int *x, *y; ((char *)&x)-((char *)&y); }));
  printf("OK\n");
  return 0;
}
