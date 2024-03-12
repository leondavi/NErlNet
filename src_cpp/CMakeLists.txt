#****************************************************
# Authors: David Leon
# 29/10/2021
# 
# @copyright Copyright (c) 2021 Nerlnet
# *****************************************************/ 

project(src_cpp)

add_subdirectory(common)
add_subdirectory(opennnBridge)

if(NERLWOLF)
    message("[NERLNET] Wolfram Engine nif extension is enabled")
    add_subdirectory(wolframBridge)
endif()