#include "test.h"


int main() {

  ASSERT(10, ({ double i=10.0; int j=0; for (; i; i--, j++); j; }));

   printf("OK\n");
  return 0;
}
