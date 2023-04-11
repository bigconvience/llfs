#include "test.h"


int main() {
      int n=10; 
    int x[n+1][n+6];
     // int *p=x; 
     // for (int i = 0; i<sizeof(x)/4; i++) 
     //    p[i]=i;
      x[3];

  //ASSERT(0, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][0]; }));
  // ASSERT(5, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][5]; }));
  // ASSERT(5*16+2, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[5][2]; }));


  printf("OK\n");

  return 0;
}
