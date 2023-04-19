#define ASSERT(x, y) assert(x, y, #y)

void assert(int expected, int actual, char *code);
int printf(char *fmt, ...);
int sprintf(char *buf, char *fmt, ...);
int vsprintf(char *buf, char *fmt, void *ap);
int strcmp(char *p, char *q);
int strncmp(char *p, char *q, long n);
int memcmp(char *p, char *q, long n);
void exit(int n);
int vsprintf();
long strlen(char *s);


#ifndef __STDDEF_H
#define __STDDEF_H

#define NULL ((void *)0)

typedef unsigned long size_t;
typedef long ptrdiff_t;
typedef unsigned int wchar_t;
typedef long max_align_t;

#define offsetof(type, member) ((size_t)&(((type *)0)->member))

#endif