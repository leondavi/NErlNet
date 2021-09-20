project(opennnBridge)

set(CMAKE_VERBOSE_MAKEFILE ON) #Cmake Debug On
set(CMAKE_CXX_STANDARD 11)

set(SRC_CODE 
	"include/nifpp.h"
	"tmp.cpp")
add_library(${PROJECT_NAME} STATIC ${SRC_CODE})

set(OPENNN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../opennn/opennn/")
add_subdirectory(${OPENNN_PATH} [binary_dir]) 

target_include_directories(${PROJECT_NAME} PUBLIC ${OPENNN_PATH})

