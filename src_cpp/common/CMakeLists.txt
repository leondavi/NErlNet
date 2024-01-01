project(common)

set(SRC_CODE
    "common_definitions.h"
    "nerlLayer.cpp"
    "nerlLayer.h"
    "nerlWorker.cpp"
    "nerlWorker.h"
    "utilities.cpp"
    "utilities.h"
    "nerlWorkerFunc.h"
    "worker_definitions_ag.h"
    "bridgeController.h"
    )

add_library(common SHARED ${SRC_CODE})

target_include_directories(common PUBLIC .)