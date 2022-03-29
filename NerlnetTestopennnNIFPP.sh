#!/bin/bash

echo "Current working directory: $PWD"

echo "Compilation of NIF shared library: "
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release 
make . 

echo "Change directory to src_erl: $PWD"
cd ../../src_erl

erl -noshell -eval 'c(niftest),niftest:trainnif()' -eval 'init:stop()'
