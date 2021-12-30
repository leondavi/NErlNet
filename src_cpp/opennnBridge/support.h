#pragma once 

#include <string>
#include <iostream>

#define VERBOSE_NONE 0
#define VERBOSE_LOW 1
#define VERBOSE_MEDIUM 2
#define VERBOSE_HIGH 3

#define VERBOSITY_LEVEL VERBOSE_LOW


const std::string opennnBridgeLogPrefix = "[OPENNN_BRIDGE] ";

void printOpennnBridgeLog(char* logStr)
{
   #if VERBOSITY_LEVEL == 1
     std::cout << logStr << std::endl;
   #endif 
}
