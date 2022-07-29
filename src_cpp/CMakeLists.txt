#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(src_cpp)

add_subdirectory(opennnBridge)
add_library(${PROJECT_NAME} SHARED $<TARGET_OBJECTS:opennnBridge>)
