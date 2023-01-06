#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
      long s = (long)&g1 % 512;
  // ASSERT(0, (long)(char *)&g1 % 512);
 //  printf("OK\n");
  return 0;
}
