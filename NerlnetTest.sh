#!/bin/bash

echo "Nerlnet Test"

echo "Compilation of NIF shared library: "
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release 
make . 
cd -

echo "Change directory to src_erl:"
cd src_erl/erlBridge
echo "$PWD"

# TODO copy files to build/staging/tests directories and test it there to avoid beam files within sources!

COMPILE_NERLNIF="compile:file(\"nerlNIF.erl\")"
COMPILE_NERLTEST="compile:file(\"nerlTests.erl\")"
#c(niftest), c(nerlNIF), niftest:run_tests().
erl -noshell -eval "NerlNIF = $COMPILE_NERLNIF, NerlTests = $COMPILE_NERLTEST, io:format(\"~p,~p~n\",[NerlNIF, NerlTests]), nerlTests:run_tests()." -s init stop > test.log
cat test.log # improve log 
cd -
