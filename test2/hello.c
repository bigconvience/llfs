#include "test.h"


int main() {
  ASSERT(55, ({ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; j; }));

// ASSERT(10, ({ double i=10.0; int j=0; for (; i; i--, j++); j; }));
   printf("OK\n");
  return 0;
}
