#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(nerlnet DESCRIPTION "Nerlnet Distributed Machine Learning Research Platform")
cmake_minimum_required(VERSION 3.16.3 FATAL_ERROR)

find_package(OpenMP REQUIRED)

set(PROJECT_BINARY_DIR build)
set (CMAKE_CXX_STANDARD 11)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -latomic -fopenmp")#${OpenMP_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -latomic -fopenmp")#${OpenMP_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")

###########################
# adding sub directories 
###########################
add_subdirectory(src_cpp) # actually adding opennnBridge

add_library(${PROJECT_NAME} SHARED $<TARGET_OBJECTS:opennnBridge>)
target_link_libraries(${PROJECT_NAME} PUBLIC opennn opennnBridge)
