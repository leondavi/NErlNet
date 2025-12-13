#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(torchBridge)

set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp/")
set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")
set(COMMON_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../common")
if(DEFINED TORCH_CMAKE_PREFIX)
    set(Torch_DIR "${TORCH_CMAKE_PREFIX}")
elseif(DEFINED TORCH_ROOT)
    set(Torch_DIR "${TORCH_ROOT}/share/cmake/Torch")
endif()

find_package(Torch REQUIRED CONFIG)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS "-fpic")
set(ERL_NIF_DEFAULT_LOCATION "/usr/local/lib/erlang/usr/include")

# cpp Simple logger options
add_definitions( -D LOGGER_MAX_LOG_LEVEL_PRINTED=6 )
add_definitions( -D LOGGER_PREFIX_LEVEL=2 )
add_definitions( -D LOGGER_ENABLE_COLORS=1 )
add_definitions( -D LOGGER_ENABLE_COLORS_ON_USER_HEADER=0 )

set(SRC_CODE
    "nerltensorTorchDefs.h"
    "nifppNerlTensorTorch.h"
    "torchNIF.h"
    "torchNIF.cpp"
    "NerlWorkerTorch.h"
    "NerlWorkerTorch.cpp"
    "NerlWorkerTorchNIF.h"
	)

add_library(${PROJECT_NAME} SHARED ${SRC_CODE})

target_link_libraries(${PROJECT_NAME} "${TORCH_LIBRARIES}" common)

# Include NIF, OpenNN and Simple Cpp Logger
target_include_directories(${PROJECT_NAME} PUBLIC
							${COMMON_PATH}
							${NIFPP_PATH}
							${SIMPLE_LOGGER_PATH}
							${ERL_NIF_DEFAULT_LOCATION})