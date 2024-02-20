#****************************************************
# Authors: David Leon & Nadav Hugi
# 20/02/2024	
# 
# @copyright Copyright (c) 2023 Nerlnet
# *****************************************************/ 

project(sourceDriver)

set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")
set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp/")
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS "-fpic")
set(ERL_NIF_DEFAULT_LOCATION "/usr/local/lib/erlang/usr/include")
set(COMMON_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../common")

# cpp Simple logger options
add_definitions( -D LOGGER_MAX_LOG_LEVEL_PRINTED=6 )
add_definitions( -D LOGGER_PREFIX_LEVEL=2 )
add_definitions( -D LOGGER_ENABLE_COLORS=1 )
add_definitions( -D LOGGER_ENABLE_COLORS_ON_USER_HEADER=0 )


set(SRC_CODE
	"SourceApiNIF.h"
	"SourceController.h"
    "SourceController.cpp")

add_library(${PROJECT_NAME} SHARED ${SRC_CODE})
#target_link_libraries(${PROJECT_NAME} PUBLIC)

# Include NIF, OpenNN and Simple Cpp Logger
target_include_directories(${PROJECT_NAME} PUBLIC
							${NIFPP_PATH}
							${COMMON_PATH}
							${SIMPLE_LOGGER_PATH}
							${ERL_NIF_DEFAULT_LOCATION})