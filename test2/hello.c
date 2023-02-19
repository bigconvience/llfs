#include "test.h"

int main() {

  ASSERT(8, ({ struct t {int a; int b;} x; struct t y; sizeof(y); }));
  ASSERT(8, ({ struct t {int a; int b;}; struct t y; sizeof(y); }));
  ASSERT(2, ({ struct t {char a[2];}; { struct t {char a[4];}; } struct t y; sizeof(y); }));
  printf("OK\n");
  return 0;
}
