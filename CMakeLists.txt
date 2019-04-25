cmake_minimum_required(VERSION 3.10)

project(rad)

file(GLOB SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )

if(CMAKE_BUILD_TYPE MATCHES DEBUG)
  message("In DEBUG mode")
  set(LLVM_DIR /home/libarria/code/llvm/debug/lib/cmake/llvm )
  set(RAD_LINK_FLAG " ")
else()
  message("In Release mode ${CMAKE_BUILD_TYPE}")
  set(LLVM_DIR /home/libarria/code/llvm/release/lib/cmake/llvm )
  set(RAD_LINK_FLAG "-s")
endif()

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message("LLVM_INCLUDE_DIRS=${LLVM_INCLUDE_DIRS}")
message("LLVM_DEFINITIONS=${LLVM_DEFINITIONS}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})


link_directories( ${CMAKE_CURRENT_SOURCE_DIR}/dyncall/lib/linux )

add_executable(rad ${SRC_FILES})
target_include_directories(rad PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/../llvm/llvm-src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../llvm/debug/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/dyncall/include
    )

target_compile_options(rad PRIVATE -Wno-switch -Wno-format-security)

llvm_config(rad support core debuginfodwarf 
    linker native nativecodegen option passes target )

set_property(TARGET rad APPEND_STRING PROPERTY LINK_FLAGS " ${RAD_LINK_FLAG}")

target_link_libraries(rad PRIVATE stdc++ z rt tinfo curses xml2 m dl pthread )

target_link_libraries(rad PRIVATE dyncall_s dynload_s)


