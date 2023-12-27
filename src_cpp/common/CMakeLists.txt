project(common)

set(SRC_CODE
    "nerlLayer.cpp"
    "nerlLayer.h"
    "nerlWorker.cpp"
    "nerlWorker.h"
    "utilities.cpp"
    "utilities.h"
    "nerlWorkerFunc.h"
    )

add_library(common SHARED ${SRC_CODE})

target_include_directories(common PUBLIC .)