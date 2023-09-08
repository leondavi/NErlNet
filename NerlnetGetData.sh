#!/bin/bash



NERLNET_LIB="/usr/local/lib/nerlnet-lib/NErlNet/"
NERLNET_TMP="/tmp/nerlnet"
NERLNET_PREFIX="[GET_DATA]"
INPUT_DATA_DIR="$NERLNET_TMP/data"

#GITHUB_REPO_HARAN="https://github.com/halfway258/NerlnetData/archive/refs/heads/main.zip"
GITHUB_REPO_DEFAULT="https://github.com/leondavi/NerlnetData/archive/refs/heads/master.zip"
NERLNET_DATA_UNZIP_DIRECTORY="/tmp/nerlnet/data/NerlnetData-master/nerlnet"
REPO_ZIP="master.zip"
if [ -f "$REPO_ZIP" ]; then
        echo "$NERLNET_PREFIX old $REPO_ZIP removed!"
	rm $REPO_ZIP
fi

mkdir -p $NERLNET_TMP

if [ -d "$INPUT_DATA_DIR" ]; then
        echo "$NERLNET_PREFIX deleting old input data directory at $INPUT_DATA_DIR"
        rm -rf $INPUT_DATA_DIR
        echo "$NERLNET_PREFIX $INPUT_DATA_DIR is being generated and filled with default data"
        echo "$NERLNET_PREFIX wget runs in quiet mode"
        wget -q $GITHUB_REPO_DEFAULT
        unzip master.zip -d $INPUT_DATA_DIR
        rm master.zip
        echo "$NERLNET_PREFIX Add input data to $INPUT_DATA_DIR"
else
        echo "$NERLNET_PREFIX $INPUT_DATA_DIR is being generated and filled with default data"
        echo "$NERLNET_PREFIX wget runs in quiet mode"
        wget -q $GITHUB_REPO_DEFAULT
        unzip master.zip -d $INPUT_DATA_DIR
        rm master.zip
        echo "$NERLNET_PREFIX Add input data to $INPUT_DATA_DIR"
fi

echo "$NERLNET_PREFIX data is located at $NERLNET_DATA_UNZIP_DIRECTORY"