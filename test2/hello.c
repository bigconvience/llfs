#include "test.h"


int main() {
  ASSERT(1, 3&1);
  ASSERT(3, 7&3);
  ASSERT(10, -1&10);
  
  printf("OK\n");
  return 0;
}
