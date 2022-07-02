# llfs: Learn LLVM from scratch

## Purpose
Build a simple C compiler based on [chibicc cpp version](https://github.com/bigconvience/chibicc_cpp/) ast and backend by LLVM. I create this project to learn LLVM.

## LLVM Commands to use
[CommandGuide](https://llvm.org/docs/CommandGuide/)

clang hello.c -o a.out
clang -emit-llvm -S hello.c -o hello.ll
clang -Xclang -ast-dump -c initializer.c

clang++ hello.cpp -o a.out
clang++ -emit-llvm -S hello.cpp -o hello.ll

llvm-as hello.ll -o hello.bc

llvm-dis hello.bc -o hello.ll

llc hello.ll -o hello.s

lli hello.ll
