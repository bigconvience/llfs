#include "test.h"

void *fn(int x, void *p, int y) { return p; }

int main() {
  int i = 0;

  char *p1 = alloca(16);
  char *p2 = alloca(16);
 char *p3 = 1 + (char *)alloca(3) + 1;
 p3 = p3 - 2;
 char *p4 = fn(1, alloca(16), 3);


  memcpy(p1, "0123456789abcdef", 16);
  
  printf("OK\n");
  return 0;
}
