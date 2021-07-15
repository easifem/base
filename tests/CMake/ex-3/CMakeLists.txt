# This program is part of easifem distribution
#  2021 (c), Vikas Sharma, Ph.D.
# All rights reserved
# log (dd/mm/yyyy)
# 17-02-2021 this document was created
#---------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.13.0 FATAL_ERROR)
set(project_name "ex_2")
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

add_library(${project_name}obj
  MODULE
  mod2.f90
  mod1.f90
)
set_target_properties(${project_name}obj
  PROPERTIES
  POSITION_INDEPENDENT_CODE 1
)
add_library(
  ${project_name}lib STATIC $<TARGET_objECTS:${project_name}obj>)

add_executable(main main.f90)
target_link_libraries(main ${project_name}lib)
