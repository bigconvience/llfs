#include "test.h"


int main() {
  //int y = ({int x = 1; x > 2; });
  // int y = ({int x = 1; x + 2; });
  ASSERT(20, ({ int x; int *p=&x; p+20-p; }));
  ASSERT(1, ({ int x; int *p=&x; p+20-p>0; }));
 ASSERT(-20, ({ int x; int *p=&x; p-20-p; }));
  ASSERT(1, ({ int x; int *p=&x; p-20-p<0; }));
  printf("OK\n");
  return 0;
}
