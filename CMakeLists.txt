# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

cmake_minimum_required(VERSION 3.13.0 FATAL_ERROR)
set(PROJECT_NAME "easifemBase")
project(${PROJECT_NAME})
enable_language(C)
enable_language(Fortran)
enable_language(CXX)
set(VERSION_MAJOR "1")
set(VERSION_MINOR "1")
set(VERSION_BugFix "0")
set(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})
include(CMakePrintHelpers)

######################################################################
#
# SYSTEM INFORMATION
#
#####################################################################
cmake_print_variables(CMAKE_SYSTEM)
cmake_print_variables(CMAKE_SYSTEM_NAME)
cmake_print_variables(CMAKE_SYSTEM_VERSION)
cmake_print_variables(CMAKE_SYSTEM_PROCESSOR)
cmake_print_variables(CMAKE_HOST_SYSTEM_NAME)
#-------------------------------------------------------------------

option(BUILD_SHARED_LIBS "Build shared library" ON)
if (BUILD_SHARED_LIBS)
  message("-- EASIFEM will be built as a shared library.")
  set(${PROJECT_NAME}_LIB_EXPORT libeasifemBase.dylib)
else()
  message("-- EASIFEM will be built as a static library.")
  set(${PROJECT_NAME}_LIB_EXPORT libeasifemBase.a)
endif()

######################################################################
#
# COMPILER RELATED OPTIONS
#
######################################################################
#
message(STATUS "Is the Fortran compiler loaded? ${CMAKE_Fortran_COMPILER_LOADED}")
if(CMAKE_Fortran_COMPILER_LOADED)
  message(STATUS "Fortran compiler ID = ${CMAKE_Fortran_COMPILER_ID}")
  message(STATUS "Is the Fortran from GNU? ${CMAKE_COMPILER_IS_GNUFortran}")
  message(STATUS "The Fortran compiler version is: ${CMAKE_Fortran_COMPILER_VERSION}")
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

if( CMAKE_Fortran_COMPILER_ID MATCHES GNU)
	list(APPEND FORTRAN_FLAGS "-O2" "-cpp" "-fPIC")
	list(APPEND FORTRAN_FLAGS_DEBUG "-g" "-Wextra" "-Wall" "-std=f2008" "-pedantic" "-cpp" "-fPIC" "-fall-intrinsics")
	list( APPEND FORTRAN_FLAGS_RELEASE "-O2" "-cpp" "-fPIC")
endif()

######################################################################
#
# Find packages
#
######################################################################
#
#--------------------- BLAS and LAPACK ---------------------

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
endif()

#download openblas using homebrew on macOS
if( NOT BLAS_LIBRARIES AND CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(BLAS_LIBRARIES "/usr/local/opt/openblas/lib/libopenblas.dylib" CACHE PATH "libblas")
endif()
#download lapack
if( NOT LAPACK_LIBRARIES AND CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(LAPACK_LIBRARIES "/usr/local/opt/lapack/lib/liblapack.dylib" CACHE PATH "liblapack" FORCE)
endif()
#download openblas using sudo-apt on Ubuntu
if( NOT BLAS_LIBRARIES AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(BLAS_LIBRARIES "/usr/lib/libblas.so" CACHE PATH "libblas" FORCE)
endif()
#download lapack
if( NOT LAPACK_LIBRARIES AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(LAPACK_LIBRARIES "/usr/lib/liblapack.so" CACHE PATH "liblapack" FORCE)
endif()
cmake_print_variables(BLAS_LIBRARIES)
cmake_print_variables(LAPACK_LIBRARIES)

#--------------------- HDF5 ------------------------------------------
find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
if(HDF5_FOUND)
  cmake_print_variables(HDF5_VERSION HDF5_INCLUDE_DIRS)
else()
  message("HDF5 library not found")
endif()

######################################################################
#
# CREATING DIRECTORIES FOR EASIFEM_BASE
#
######################################################################
#
# ------------------------windows-----------------------------------
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  message(STATUS "Windows system found")
else()
# ------------------------unit/linux--------------------------------
  set(EASIFEM_DEFAULT_PREFIX "${CMAKE_BINARY_DIR}/../../../")
  set(EASIFEM_PREFIX "${EASIFEM_DEFAULT_PREFIX}" CACHE PATH "location where easifem will be installed")
  message("EASIFEM_PREFIX = ${EASIFEM_PREFIX}")

  if( EXISTS "${EASIFEM_PREFIX}/easifem")
    message("${EASIFEM_PREFIX}/easifem already exists, using it only")
  else()
    message("${EASIFEM_PREFIX}/easifem has been created")
    file( MAKE_DIRECTORY "${EASIFEM_PREFIX}/easifem" )
  endif()
  set(EASIFEM_BASE_ARCH "${EASIFEM_PREFIX}/easifem/easifem-base" CACHE PATH "Location of easifem-base")

  if( EXISTS "${EASIFEM_BASE_ARCH}")
    message("${EASIFEM_BASE_ARCH} already exists, using it.")
  else()
    message("${EASIFEM_BASE_ARCH} has been created")
    file( MAKE_DIRECTORY "${EASIFEM_BASE_ARCH}" )
  endif()
  set(EASIFEM_BASE_INCLUDE "${EASIFEM_BASE_ARCH}/include" CACHE PATH "Location where modules of easifemBase")

  if( EXISTS "${EASIFEM_BASE_INCLUDE}")
    message("${EASIFEM_BASE_INCLUDE} already exists, using it.")
  else()
    message("${EASIFEM_BASE_INCLUDE} has been created")
    file( MAKE_DIRECTORY "${EASIFEM_BASE_INCLUDE}" )
  endif()
  set(EASIFEM_BASE_LIB "${EASIFEM_BASE_ARCH}/lib" CACHE PATH "Location where modules of easifemBase")

  if( EXISTS "${EASIFEM_BASE_LIB}")
    message("${EASIFEM_BASE_LIB} already exists, using it.")
  else()
    message("${EASIFEM_BASE_LIB} has been created")
    file( MAKE_DIRECTORY "${EASIFEM_BASE_LIB}" )
  endif()
endif()
set(CMAKE_Fortran_MODULE_DIRECTORY "${EASIFEM_BASE_INCLUDE}")

######################################################################
#
# INSTALLING easifemBase
#
######################################################################

add_subdirectory(src/modules)