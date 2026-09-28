# SPDX-License-Identifier: AGPL-3.0-or-later
# Standalone `siu` CLI + C++ tests, built from the package's own C++ core in
# ../../src (src/siu/*.hpp headers, src/siu_core_*.cpp sources), so the CLI and
# the R package can never drift. Build:
#   cmake -S tools/siu-cli -B build/siu && cmake --build build/siu && ctest --test-dir build/siu
cmake_minimum_required(VERSION 3.16)
project(siu LANGUAGES CXX VERSION 1.0.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

find_package(CURL REQUIRED)
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

set(PKG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/../../src)
add_library(siucore STATIC
  ${PKG_SRC}/siu_core_parse.cpp ${PKG_SRC}/siu_core_resolve.cpp
  ${PKG_SRC}/siu_core_http.cpp ${PKG_SRC}/siu_core_llm.cpp ${PKG_SRC}/siu_core_audit.cpp)
target_include_directories(siucore PUBLIC ${PKG_SRC})
target_link_libraries(siucore PUBLIC CURL::libcurl Threads::Threads)

add_executable(siu main.cpp)
target_link_libraries(siu PRIVATE siucore)

enable_testing()
foreach(t resolve panel parse llm)
  add_executable(test_${t} tests/test_${t}.cpp)
  target_link_libraries(test_${t} PRIVATE siucore)
  target_compile_options(test_${t} PRIVATE -UNDEBUG)  # asserts stay live in Release
  add_test(NAME ${t} COMMAND test_${t})
endforeach()

install(TARGETS siu RUNTIME DESTINATION bin)
