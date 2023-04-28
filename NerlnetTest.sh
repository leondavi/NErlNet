#!/bin/bash

echo "Nerlnet Test"

echo "Compilation of NIF shared library: "
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release 
make . 
cd -

set -x

echo "Change directory to src_erl: $PWD"
cd src_erl/erlBridge

erl -noinput -s niftest run_tests -s init stop > test.log
cat test.log

cd -
