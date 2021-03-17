# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 22/02/2021 this file was created
#-----------------------------------------------------------------------

cmake_minimum_required(VERSION 3.19.0 FATAL_ERROR)
set(PROJECT_NAME "VoigtRank2Tensor")
project(${PROJECT_NAME})
enable_language(Fortran)
set(TARGET_NAME "test")

add_executable(
  ${TARGET_NAME}
  test_${PROJECT_NAME}.f90
)

list(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_BASE}/share/cmake/")
find_package(OpenMP)
find_package(easifemBase 1.1.0 CONFIG REQUIRED)
list( APPEND TARGET_COMPILE_DEF "-DUSE_OpenMP" )
list( APPEND TARGET_COMPILE_OPT ${OpenMP_Fortran_FLAGS}  )

target_compile_options(
  ${TARGET_NAME}
  PRIVATE
  ${TARGET_COMPILE_OPT}
  ${CMAKE_Fortran_FLAGS}
)

target_compile_definitions(
  ${TARGET_NAME}
  PUBLIC
  ${TARGET_COMPILE_DEF}
)

get_target_property(
    easifemBase_INCLUDEDIR
    easifemBase::easifemBase
    INTERFACE_INCLUDE_DIRECTORIES)

target_include_directories(
    ${TARGET_NAME}
    PRIVATE
    ${easifemBase_INCLUDEDIR}
  )

target_link_libraries(
    ${TARGET_NAME}
    PRIVATE
    easifemBase::easifemBase
    "$<$<BOOL:${OpenMP_FOUND}>:${OpenMP_Fortran_LIBRARIES}>"
  )
