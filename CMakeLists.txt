#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

cmake_minimum_required(VERSION 3.16.3 FATAL_ERROR)
project(nerlnet DESCRIPTION "Nerlnet Distributed Machine Learning Research Platform")

set(PROJECT_BINARY_DIR build)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -latomic")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -latomic")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")

# Options
option(NERLWOLF "Use Wolfram Engine workers extension" OFF)
option(NERLTORCH "use libtorch installed to /usr/local/lib/libtorch" OFF)
option(USE_OpenMP "Use-OpenMP" ON)

#add_compile_definitions(EIGEN_MAX_ALIGN_BYTES=8) #Open this line for RASPI
if(USE_OpenMP)
    find_package(OpenMP)
    if(OPENMP_FOUND)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    endif()
endif()

###########################
# adding sub directories 
###########################
add_subdirectory(src_cpp) # actually adding opennnBridge

add_library(nerlnet_onn SHARED $<TARGET_OBJECTS:opennnBridge>)
target_link_libraries(nerlnet_onn PUBLIC opennnBridge)

add_library(source_nif SHARED $<TARGET_OBJECTS:sourceNIF>)
target_link_libraries(source_nif PUBLIC sourceNIF)

if (NERLWOLF)
    add_library(nerlnet_wolf SHARED $<TARGET_OBJECTS:wolframBridge>)
    target_link_libraries(nerlnet_wolf PUBLIC wolframBridge)
endif()

if (NERLTORCH)
    add_library(nerlnet_torch SHARED $<TARGET_OBJECTS:torchBridge>)
    target_link_libraries(nerlnet_torch PUBLIC torchBridge)
endif()