#include "test.h"

int sum(int a) {
  int b;
  unsigned int c;
  unsigned int d;
  a + b;
  c + d;
  return -1024;
}

int main() { 
  struct bit1 {
    short a;
    char b;
    int c : 2;
    int d : 3;
    int e : 3;
  };

    ASSERT(1, ({ struct bit1 x={1,2,3,4,5}; x.a; }));

  return 0;
}
