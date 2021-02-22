# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 22/02/2021 this file was created
#-----------------------------------------------------------------------

cmake_minimum_required(VERSION 3.14.0 FATAL_ERROR)
set(PROJECT_NAME "GlobalData")
project(${PROJECT_NAME})
enable_language(Fortran)

add_executable(test_${PROJECT_NAME}
  test_${PROJECT_NAME}.f90
)

if(DEFINED ENV{EASIFEM_CMAKE_PREFIX_PATH})
  # list(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_CMAKE_PREFIX_PATH}")
  list(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_BASE}/share/cmake/")
endif()

find_package(easifemBase 1.1.0 CONFIG REQUIRED)

if(easifemBase_FOUND)
  message(STATUS "Found easifemBase")
  target_include_directories(
    test_${PROJECT_NAME}
    PRIVATE
  )

  target_link_libraries(
    test_${PROJECT_NAME}
    PRIVATE
    easifemBase::easifemBase
  )

else()
  message(ERROR " easifemBase not found")
endif()