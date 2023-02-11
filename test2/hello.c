#include "test.h"


int main() {

  //ASSERT(3, (1,2,3));
  //ASSERT(5, ({ int i=2, j=3; (i=5,j)=6; i; }));
  //ASSERT(6, ({ int i=2, j=3; (i=5,j)=6; j; }));

  // ASSERT(55, ({ int j=0; for (int i=0; i<=10; i=i+1) j=j+i; j; }));
  // ASSERT(3, ({ int i=3; int j=0; for (int i=0; i<=10; i=i+1) j=j+i; i; }));

  ASSERT(3, ({ int i=0; for(;i<10;i++) { for (;;) break; if (i == 3) break; } i; }));
   printf("OK\n");
  return 0;
}
