#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(opennnBridge)

set(EIGEN3_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/eigen")
set(CMAKE_VERBOSE_MAKEFILE ON) #Cmake Debug On
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-fpic")
#set(CMAKE_CXX_FLAGS "g")
#set(CMAKE_CXX_FLAGS "-ggdb")

include_directories(${EIGEN3_INCLUDE_DIR})
message("-- [OPENNN_BRIDGE] Eigen Include Directory: ${EIGEN3_INCLUDE_DIR}")


#set(SRC_CODE 
#	"ModelParams.cpp"
#	"include/nifpp.h"
#	"include/definitionsNN.h"
#	"include/nifppEigenExtensions.h"
#	"tmp.cpp")
#add_library(${PROJECT_NAME} STATIC ${SRC_CODE})

set(SRC_CODE 
	"nifpp.h"
	"nifppEigenExtensions.h"
	"bridgeController.h"
	"definitionsNN.h"
	"train.h"
	"CustumNN.h"
	"openNNnif.h"
    "ModelParams.h"	
	"Autoencoder.h"
	"ModelParams.cpp"
	"definitionsNN.h"
	"tmp.cpp")
add_library(${PROJECT_NAME} STATIC ${SRC_CODE})

set(OPENNN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/opennn/")
add_subdirectory(${OPENNN_PATH} [binary_dir]) 

target_include_directories(${PROJECT_NAME} PUBLIC ${OPENNN_PATH} ${EIGEN3_INCLUDE_DIR})
