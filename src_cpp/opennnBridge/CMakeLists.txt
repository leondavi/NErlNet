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
#	"ModelParams.cpp"
#	"tmp.cpp")
#add_library(${PROJECT_NAME} STATIC ${SRC_CODE})

set(SRC_CODE 
	"ModelParams.cpp"
	"nifpp.h"
	"definitionsNN.h"
	"nifppEigenExtensions.h"
	"ModelParams.cpp"
	"tmp.cpp")
add_library(${PROJECT_NAME} STATIC ${SRC_CODE})

set(OPENNN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/opennn/")
add_subdirectory(${OPENNN_PATH} [binary_dir]) 

target_include_directories(${PROJECT_NAME} PUBLIC ${OPENNN_PATH})

