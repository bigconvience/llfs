#include "test.h"

int main() {

  ASSERT(2, ({ int x; if (2-1) x=2; else x=3; x; }));
  ASSERT(55, ({ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; j; }));
  ASSERT(10, ({ int i=0; while(i<10) i=i+1; i; }));

  printf("OK\n");
  return 0;
}
