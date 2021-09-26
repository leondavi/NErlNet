#!/bin/bash

#Builds 

git clone https://github.com/zivmo99/inputDataFiles.git

cd src_py
python3 BuildScript.py cpp=True
