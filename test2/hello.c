#include "test.h"


int main() {

  //ASSERT(3, (1,2,3));
  //ASSERT(5, ({ int i=2, j=3; (i=5,j)=6; i; }));
  //ASSERT(6, ({ int i=2, j=3; (i=5,j)=6; j; }));

  ASSERT(55, ({ int j=0; for (int i=0; i<=10; i=i+1) j=j+i; j; }));
  ASSERT(3, ({ int i=3; int j=0; for (int i=0; i<=10; i=i+1) j=j+i; i; }));

 ASSERT(10, ({ int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } i; }));
 ASSERT(6, ({ int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } j; }));
  ASSERT(10, ({ int i=0; int j=0; for(;!i;) { for (;j!=10;j++) continue; break; } j; }));

  printf("OK\n");
  return 0;
}
