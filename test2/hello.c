#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
ASSERT(0, ({ char x[16]; (unsigned long)&x % 16; }));
  // ASSERT(0, ({ char x[17]; (unsigned long)&x % 16; }));
  //ASSERT(0, ({ char x[100]; (unsigned long)&x % 16; }));
  //ASSERT(0, ({ char x[101]; (unsigned long)&x % 16; }));
  // printf("OK\n");
  return 0;
}
