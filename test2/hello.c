#include "test.h"


int main() {
  int a[3];
  int *p=a+1; ++*p;


  printf("OK\n");
  return 0;
}
