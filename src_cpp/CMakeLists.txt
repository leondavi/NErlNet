project(src_cpp)

add_subdirectory(opennnBridge)
add_library(${PROJECT_NAME} SHARED $<TARGET_OBJECTS:opennnBridge>)
