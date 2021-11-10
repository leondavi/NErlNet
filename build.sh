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

cp inputDataFiles/RunOrWalkPredictNolabels_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
cp inputDataFiles/RunOrWalkTrain_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
cp inputDataFiles/RunOrWalkPredictNolabelsNormalized_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
cp inputDataFiles/RunOrWalkTrainNormalized_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/

cd src_py
python3 BuildScript.py cpp=True
cd ..

echo "Script PWD: $PWD"

cd src_erl/Communication_Layer/http_Nerlserver
../../rebar3/rebar3 shell 
cd ../../../
