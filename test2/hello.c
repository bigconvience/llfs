#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
   int *p1 = alloca(16);
  printf("OK\n");
  return 0;
}
