#include "test.h"

    struct {
  char a;
  int b : 5;
  int c : 10;
} g45 = {1, 2, 3}, g46={};

int main() { 


      struct bit1 {
    short a;
    char b;
    int c : 2;
    int d : 3;
    int e : 3;
  };

int k = 5;
struct bit1 x; x.a=k; 

  return 0;
}
