#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
  ASSERT(-8, ({ _Alignas(long) char x, y; &y-&x; }));
  ASSERT(-32, ({ _Alignas(32) char x, y; &y-&x; }));
  printf("OK\n");
  return 0;
}
