#!/bin/bash

rm toy.out

clang++ ir_gen.cpp llfs.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -fno-rtti -o toy.out
./toy.out