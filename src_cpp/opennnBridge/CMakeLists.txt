#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(opennnBridge)

set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp/")
set(OPENNN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/opennn/")
set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")
set(COMMON_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../common")

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS "-fpic")

# cpp Simple logger options
add_definitions( -D LOGGER_MAX_LOG_LEVEL_PRINTED=6 )
add_definitions( -D LOGGER_PREFIX_LEVEL=2 )
add_definitions( -D LOGGER_ENABLE_COLORS=1 )
add_definitions( -D LOGGER_ENABLE_COLORS_ON_USER_HEADER=0 )

add_subdirectory(${OPENNN_PATH} [binary_dir])

set(SRC_CODE
	"eigenTensorTypes.h"
	"nifppEigenExtensions.h"
	"nifppNerltensorEigen.h"
	"openNNnif.h"
	"openNNnif.cpp"
	"nerlWorkerOpenNN.h"
	"nerlWorkerOpenNN.cpp"
	"nerlWorkerNIF.h"
	"ae_red.h"
	"ae_red.cpp"
	"encode_nerltensor_nif.h"
	"decode_nerltensor_nif.h"
	"operations_nerltensor_nif.h"
	)

add_library(${PROJECT_NAME} SHARED ${SRC_CODE})

target_link_libraries(${PROJECT_NAME} PUBLIC opennn common)

# Include NIF, OpenNN and Simple Cpp Logger
target_include_directories(${PROJECT_NAME} PUBLIC
							${COMMON_PATH}
							${OPENN_PATH}
							${SIMPLE_LOGGER_PATH}
							)