#!/bin/bash

rm *.out

clang++ main.c irgen.cpp codegen.cpp  `llvm-config --cxxflags --ldflags --system-libs --libs core` -fno-rtti -o toy.out
./toy.out