#define ASSERT(x, y) assert(x, y, #y)

void assert(int expected, int actual, char *code);
int printf(const char *fmt, ...);
void exit(int n);
