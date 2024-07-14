project(common)

set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp/")
set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")

set(ERL_NIF_DEFAULT_LOCATION "/usr/local/lib/erlang/usr/include")

set(SRC_CODE
    "nerltensor.h"
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

target_include_directories(common PUBLIC .
    ${NIFPP_PATH}
    ${SIMPLE_LOGGER_PATH}
    ${ERL_NIF_DEFAULT_LOCATION}
)