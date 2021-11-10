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

git clone https://github.com/zivmo99/inputDataFiles.git

mv inputDataFiles/RunOrWalkPredictNolabels_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
mv inputDataFiles/RunOrWalkTrain_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
mv inputDataFiles/RunOrWalkPredictNolabelsNormalized_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
mv inputDataFiles/RunOrWalkTrainNormalized_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/

cd src_py
python3 BuildScript.py cpp=True
