
project(SourceNIF)

set(CMAKE_CXX_STANDARD 17)
set(NIFPP_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../nifpp/")
set(COMMON_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../common")
set(SIMPLE_LOGGER_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../simple-cpp-logger/include")

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS "-fpic")
set(ERL_NIF_DEFAULT_LOCATION "/usr/local/lib/erlang/usr/include")

# cpp Simple logger options
add_definitions( -D LOGGER_MAX_LOG_LEVEL_PRINTED=6 )
add_definitions( -D LOGGER_PREFIX_LEVEL=2 )
add_definitions( -D LOGGER_ENABLE_COLORS=1 )
add_definitions( -D LOGGER_ENABLE_COLORS_ON_USER_HEADER=0 )

set(SRC_CODE
    "Source.h"
    "SourceNIF.h"
    "SourceCSV.h"
    "SourceCSV.cpp"
)


add_library(${PROJECT_NAME} SHARED ${SRC_CODE})

target_link_libraries(${PROJECT_NAME} PUBLIC common)

# Include NIF, OpenNN and Simple Cpp Logger
target_include_directories(${PROJECT_NAME} PUBLIC
							${COMMON_PATH}
							${NIFPP_PATH}
							${SIMPLE_LOGGER_PATH}
							${ERL_NIF_DEFAULT_LOCATION})