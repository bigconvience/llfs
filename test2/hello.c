#include "test.h"


int main() {
  ASSERT(5, ((long)17)%6);
  ASSERT(2, ({ int i=10; i%=4; i; }));
  ASSERT(2, ({ long i=10; i%=4; i; }));

  
  printf("OK\n");
  return 0;
}
