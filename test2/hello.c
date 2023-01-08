#include "test.h"


int main() {
  ASSERT(3, ({ int i=2; ++i; }));

  printf("OK\n");
  return 0;
}
