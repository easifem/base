# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 22/02/2021 this file was created
#-----------------------------------------------------------------------

cmake_minimum_required(VERSION 3.19.0 FATAL_ERROR)
set(PROJECT_NAME "BLAS1V")
project(${PROJECT_NAME})
enable_language(Fortran)
set(TARGET_NAME "test")
set(LIB_NAME "easifemBase")

add_executable(
  ${TARGET_NAME}
  test_${PROJECT_NAME}.f90
)


list(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_BASE}/share/cmake/")
find_package(${LIB_NAME} 1.1.0 CONFIG REQUIRED)
list(APPEND CMAKE_Fortran_FLAGS "-cpp")

# INTERFACE_LINK_LIBRARIES

if(${LIB_NAME}_FOUND)

  get_target_property(
    INCLUDEDIR
    ${LIB_NAME}::${LIB_NAME}
    INTERFACE_INCLUDE_DIRECTORIES)

  target_include_directories(
    ${TARGET_NAME}
    PRIVATE
    ${INCLUDEDIR}
  )

  message(STATUS "debug:: ${LIB_NAME}::${LIB_NAME}")

  target_link_libraries(
    ${TARGET_NAME}
    PUBLIC
    ${LIB_NAME}::${LIB_NAME}
  )

else()
  message(ERROR " ${LIB_NAME} not found")
endif()