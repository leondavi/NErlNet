#!/bin/bash

NERLNET_LIB="/usr/local/lib/nerlnet-lib/NErlNet/"
NERLNET_PREFIX="[GET_DATA]"
INPUT_DATA_DIR="inputDataDir"

if [ -d "$INPUT_DATA_DIR" ]; then
        echo "$NERLNET_PREFIX Input data directory of nerlnet is: $INPUT_DATA_DIR"
else
        echo "$NERLNET_PREFIX $INPUT_DATA_DIR is generated and is empty!"
        wget https://github.com/halfway258/NerlnetData/archive/refs/heads/main.zip
        unzip main.zip -d $INPUT_DATA_DIR
        rm main.zip
        echo "$NERLNET_PREFIX Add input data to $INPUT_DATA_DIR"
fi
