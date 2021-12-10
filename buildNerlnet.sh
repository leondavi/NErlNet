#!/bin/bash

echo "Building Nerlnet Library"
echo "Cmake command of Nerlnet NIFPP"
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release
echo "Script CWD: $PWD"
echo "Build Nerlnet"
make -j4 
cd ../../
echo "Script CWD: $PWD"


