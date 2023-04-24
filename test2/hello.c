// #include "test.h"

typedef struct Tree {
  int val;
  struct Tree *lhs;
  struct Tree *rhs;
} Tree;

Tree *t1 = &(Tree){ 3, 0, 0 };

// int main() {

//   printf("OK\n");
//   return 0;
// }
