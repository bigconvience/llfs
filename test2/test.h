#define ASSERT(x, y) assert(x, y, #y)

void assert(int expected, int actual, char *code);
int printf(const char *fmt, ...);
void exit(int n);
void * memcpy( void * destination, const void * source, long num );
int memcmp(char *p, char *q, long n);
int strcmp(char *p, char *q);

#ifndef __STDDEF_H
#define __STDDEF_H

#define NULL ((void *)0)

typedef unsigned long size_t;
typedef long ptrdiff_t;
typedef unsigned int wchar_t;
typedef long max_align_t;

#define offsetof(type, member) ((size_t)&(((type *)0)->member))

#endif