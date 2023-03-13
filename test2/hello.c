#include "test.h"

struct T0 {
  char a;
  int b : 5;
  int c: 28;
} g45 = {1, 2, 3 };

struct T1 {
  char a;
  int b : 5;

  int c: 4;

} g46 = {1, 2, 3};

   struct T3 {
    int a : 10;
    int b : 10;
    int c : 10;
  } g47 = {1, 2, 3};

struct bit1 {
    char a;
    short b;
    int c : 2;
    int d : 3;
    int e : 28;
  } x = { 1, 2, 3, 9, 20 };

int main() { 

  ASSERT(1, g45.b);
  ASSERT(2, g46.b);
  ASSERT(3, g47.c);

ASSERT(3, x.c);
    return 0;
}
