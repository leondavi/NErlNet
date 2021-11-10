#!/bin/bash

REBAR3_FILE=src_erl/rebar3/rebar3

if [ -f "$REBAR3_FILE" ]; then
	echo "rebar3 exists"
else 
	echo "rebar3 Builder"
	cd src_erl/rebar3
	./bootstrap
	cd ../../	
fi

#Builds 

cd src_py
python3 BuildScript.py cpp=True
