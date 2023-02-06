#include "test.h"

float g40 = 1.5;
double g41 = 0.0 ? 55 : (0, 1 + 1 * 5.0 / 2 * (double)2 * (int)2.0);


int main() {
  ASSERT(0, ({ _Bool x=0; x; }));
  // ASSERT(1, ({ _Bool x=1; x; }));
  // ASSERT(1, ({ _Bool x=2; x; }));
  // ASSERT(1, (_Bool)1);
  // ASSERT(1, (_Bool)2);
  // ASSERT(0, (_Bool)(char)256);
   printf("OK\n");
  return 0;
}
