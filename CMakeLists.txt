cmake_minimum_required(VERSION 3.10)

project(rad)

set(CMAKE_CXX_STANDARD 17)

file(GLOB SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )
list(FILTER SRC_FILES EXCLUDE REGEX ".*libraryfinder.cpp$")

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "$ENV{HOME}" CACHE PATH "default install path" FORCE )
endif()

if(CMAKE_BUILD_TYPE MATCHES DEBUG)
  message("In DEBUG mode")
  set(LLVM_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../llvm-build/release/lib/cmake/llvm )
  set(RAD_LINK_FLAG " ")
else()
  message("In Release mode ${CMAKE_BUILD_TYPE}")
  set(LLVM_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../llvm-build/release/lib/cmake/llvm )
  set(RAD_LINK_FLAG "-s")
endif()

find_package(LLVM 11.1.0 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message("LLVM_INCLUDE_DIRS=${LLVM_INCLUDE_DIRS}")
message("LLVM_DEFINITIONS=${LLVM_DEFINITIONS}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})


link_directories( ${CMAKE_CURRENT_SOURCE_DIR}/dyncall/lib/linux )

add_executable(rad ${SRC_FILES})
target_include_directories(rad PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/../llvm-project/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../llvm-build/release/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/dyncall/include
    )

target_compile_options(rad PRIVATE -Wno-switch -Wno-format-security)

llvm_config(rad support core debuginfodwarf 
    linker native nativecodegen option passes target )

set_property(TARGET rad APPEND_STRING PROPERTY LINK_FLAGS " ${RAD_LINK_FLAG}")

target_link_libraries(rad PRIVATE stdc++ stdc++fs z rt tinfo curses xml2 m dl pthread )

target_link_libraries(rad PRIVATE dyncall_s dynload_s)

add_library(Basic SHARED modules/Basic.cpp)
set_property(TARGET Basic PROPERTY POSITION_INDEPENDENT_CODE ON)
set_target_properties(Basic PROPERTIES PREFIX "")

install(TARGETS rad DESTINATION bin)
install(TARGETS Basic DESTINATION lib)

