#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(opennnBridge)

set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp")
set(OPENNN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/opennn/")
set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS "-fpic")
set(ERL_NIF_DEFAULT_LOCATION "/usr/local/lib/erlang/usr/include")

# Include NIF, OpenNN and Simple Cpp Logger
include_directories(
	${NIFPP_PATH}
	${OPENN_PATH}
	${SIMPLE_LOGGER_PATH}
	${ERL_NIF_DEFAULT_LOCATION})

# cpp Simple logger options
add_definitions( -D LOGGER_MAX_LOG_LEVEL_PRINTED=6 )
add_definitions( -D LOGGER_PREFIX_LEVEL=2 )
add_definitions( -D LOGGER_ENABLE_COLORS=1 )
add_definitions( -D LOGGER_ENABLE_COLORS_ON_USER_HEADER=0 )

add_subdirectory(${OPENNN_PATH} [binary_dir]) 

set(SRC_CODE
	"nifppEigenExtensions.h"
	"bridgeController.h"
	"definitionsNN.h"
	"CustumNN.h"
	"openNNnif.h"
    	"ModelParams.h"	
	"openNNnif.cpp"
	"definitionsNN.h")
add_library(${PROJECT_NAME} STATIC ${SRC_CODE})


target_link_libraries(${PROJECT_NAME} PUBLIC opennn)
