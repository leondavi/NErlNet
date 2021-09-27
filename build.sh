#!/bin/bash

#Builds 

git clone https://github.com/zivmo99/inputDataFiles.git
mv inputDataFiles/RunOrWalkPredictNolabels_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/
mv inputDataFiles/RunOrWalkTrain_splitted/ ./src_erl/Communication_Layer/http_Nerlserver/input/

cd src_py
python3 BuildScript.py cpp=True
