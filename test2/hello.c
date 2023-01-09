#include "test.h"


int main() {
  int a[3];
    ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2;}));


  printf("OK\n");
  return 0;
}
