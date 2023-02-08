#include "test.h"


int main() {

   ASSERT(3, ({ int x; if (0) x=2; else x=3; x; }));

   printf("OK\n");
  return 0;
}
