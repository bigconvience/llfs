#include "test.h"

int main() {

  ASSERT(8, ({ struct {char a; int b;} x; sizeof(x); }));

 ASSERT(8, ({ struct t {int a; int b;} x; struct t y; sizeof(y); }));
  printf("OK\n");
  return 0;
}
