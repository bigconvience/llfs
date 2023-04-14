#include "test.h"


int main() {

  printf("OK\n");

  return 0;
}

char *asm_fn1(void) {
  asm("mov $50, %rax\n\t"
      "mov %rbp, %rsp\n\t"
      "pop %rbp\n\t"
      "ret");
}