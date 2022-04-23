#ifndef CHIBICC_H
#define CHIBICC_H

#include <fstream>
#include <string>
using namespace std;

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name; 

 };

void ir_gen(Obj &prog, ofstream &out);


#endif