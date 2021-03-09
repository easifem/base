# This program is part of easifem distribution
#  2021 (c), Vikas Sharma, Ph.D.
# All rights reserved
# log (dd/mm/yyyy)
# 17-02-2021 this document was created
#---------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.13.0 FATAL_ERROR)
set(project_name "ex")
project(${project_name})
enable_language(Fortran)

# Disable in-source builds to prevent source tree corruption.
if(" ${CMAKE_SOURCE_DIR}" STREQUAL " ${CMAKE_BINARY_DIR}")
  message(FATAL_ERROR "
FATAL: In-source builds are not allowed.
  You should create a separate directory for build files.
")
endif()
message(${CMAKE_SOURCE_DIR})
message(${CMAKE_BINARY_DIR})

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/include")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")

add_library(${project_name}lib
  SHARED
  mod1.f90
)
#add_library(
#  ${project_name}lib SHARED $<TARGET_OBJECTS:${project_name}obj>)

