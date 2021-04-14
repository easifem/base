# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

CMAKE_MINIMUM_REQUIRED(VERSION 3.10.0 FATAL_ERROR)
SET(PROJECT_NAME "easifemBase")
PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(C)
ENABLE_LANGUAGE(Fortran)
ENABLE_LANGUAGE(CXX)
SET(VERSION_MAJOR "1")
SET(VERSION_MINOR "0")
SET(VERSION_BugFix "0")
SET(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})
SET(CMAKE_PROJECT_DESCRIPTION "easifem base")
SET(CMAKE_PROJECT_HOMEPAGE_URL "https://github.com/")
SET(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
SET(namespace "${PROJECT_NAME}")
INCLUDE(CMakePrintHelpers)
INCLUDE(FortranCInterface)
FortranCInterface_VERIFY()

# SYSTEM INFORMATION
# Disable in-source builds to prevent source tree corruption.
IF(" ${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL " ${CMAKE_CURRENT_BINARY_DIR}")
  MESSAGE(FATAL_ERROR "
    FATAL: In-source builds are not allowed.
    You should create a separate directory for build files.")
ENDIF()

# install openblas
# list(APPEND CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_SOURCE_DIR}/cmake")
IF(NOT CMAKE_PREFIX_PATH)
  SET(CMAKE_PREFIX_PATH "$ENV{EASIFEM_EXTPKGS}")
ENDIF()

SET(BLA_VENDOR "OpenBLAS")
FIND_PACKAGE( LAPACK REQUIRED )
IF( LAPACK_FOUND )
  MESSAGE(STATUS "LAPACK_LIBRARIES found")
  MESSAGE("LAPACK_LIBRARIES: ${LAPACK_LIBRARIES}")
  MESSAGE("BLAS_LIBRARIES: ${BLAS_LIBRARIES}")
  MESSAGE("BLA_vendor: ${BLA_VENDOR}")
ENDIF()

if( BLA_VENDOR MATCHES "MKL" )
  message(STATUS "BLA_VENDOR : MKL")
  list( APPEND TARGET_COMPILE_DEF "-DUSE_INTEL_MKL" )
elseif( BLA_VENDOR MATCHES "OpenBLAS" )
  message(STATUS "BLA_VENDOR : OpenBLAS")
  list( APPEND TARGET_COMPILE_DEF "-DUSE_OpenBLAS" )
else( )
  message(STATUS "BLA_VENDOR : System provided")
  list( APPEND TARGET_COMPILE_DEF "-DUSE_NativeBLAS" )
endif( )

# Find OpenMP
option(USE_OpenMP OFF)
if(USE_OpenMP)
  find_package(OpenMP)
  if(OpenMP_FOUND)
    message("OpenMP FOUND: ")
    list( APPEND TARGET_COMPILE_DEF "-DUSE_OpenMP" )
    list( APPEND TARGET_COMPILE_OPT ${OpenMP_Fortran_FLAGS}  )
  else()
    message("OpenMP NOT FOUND: building without it")
  endif()
endif()

#Single precision or double precision
option(USE_Real32 OFF)
option(USE_Real64 OFF)

if(USE_Real32)
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Real32" )
elseif(USE_Real64)
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Real64" )
else()
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Real64" )
endif()

#Single precision or double precision
option(USE_Int32 OFF)
option(USE_Int64 OFF)
if(USE_Int32)
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Int32" )
elseif(USE_Real64)
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Int64" )
else()
  list( APPEND TARGET_COMPILE_DEF "-DUSE_Int32" )
endif()

list( APPEND TARGET_COMPILE_DEF "-D${CMAKE_HOST_SYSTEM_NAME}_SYSTEM" )

# CREATING DIRECTORIES FOR EASIFEM_BASE
include( GNUInstallDirs )
set(
  CMAKE_Fortran_MODULE_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}
)
set(
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}
)
set(
  CMAKE_ARCHIVE_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}
)
set(
  CMAKE_RUNTIME_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}
)

set(
  INSTALL_LIBDIR
  ${CMAKE_INSTALL_LIBDIR}
  CACHE PATH "install location of lib"
)
set(
  INSTALL_INCLUDEDIR
  ${CMAKE_INSTALL_INCLUDEDIR}
  CACHE PATH "install location of include"
)
set(
  INSTALL_BINDIR
  ${CMAKE_INSTALL_BINDIR}
  CACHE PATH "install location of bin"
)

if(WIN32 AND NOT CYGWIN)
  set(DEF_INSTALL_CMAKEDIR CMake)
else()
  set(DEF_INSTALL_CMAKEDIR share/cmake/${PROJECT_NAME})
endif()
set(INSTALL_CMAKEDIR ${DEF_INSTALL_CMAKEDIR} CACHE PATH "Installation directory for CMake files")

foreach(p LIB BIN INCLUDE CMAKE)
  file(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/${INSTALL_${p}DIR} _path)
  message(STATUS "Installing ${p} componenets to ${_path}")
endforeach()


option(BUILD_SHARED_LIBS "Build shared library" ON)
if (BUILD_SHARED_LIBS)
  message(STATUS "${PROJECT_NAME} will be built as a shared library.")
else()
  message(STATUS "${PROJECT_NAME} will be built as a static library.")
endif()

# INSTALLING easifemBase
if(BUILD_SHARED_LIBS)
  add_library(${PROJECT_NAME} SHARED "" )
else()
  add_library(${PROJECT_NAME} STATIC "")
endif()

# COMPILER RELATED OPTIONS
include(${PROJECT_SOURCE_DIR}/cmake/Compiler.cmake)

if( $<CONFIG:Debug> )
  list( APPEND TARGET_COMPILE_DEF "-DDEBUG_VER" )
endif()

###########################################
INCLUDE(src/modules/CMakeLists.txt)
INCLUDE(src/submodules/CMakeLists.txt)
###########################################

TARGET_COMPILE_OPTIONS(
  ${PROJECT_NAME}
  PRIVATE
  ${TARGET_COMPILE_OPT}
  ${FORTRAN_FLAGS}
  "$<$<CONFIG:Debug>:${FORTRAN_FLAGS_DEBUG}>"
  "$<$<CONFIG:Release>:${FORTRAN_FLAGS_RELEASE}>")

TARGET_COMPILE_DEFINITIONS(
  ${PROJECT_NAME}
  PUBLIC
  ${TARGET_COMPILE_DEF}
)

# target_compile_definitions()
SET_TARGET_PROPERTIES(
  ${PROJECT_NAME}
    PROPERTIES
    POSITION_INDEPENDENT_CODE 1
    SOVERSION ${VERSION_MAJOR}
    OUTPUT_NAME ${PROJECT_NAME}
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    MACOSX_RPATH ON
    WINDOWS_EXPORT_ALL_SYMBOLS ON
)

TARGET_INCLUDE_DIRECTORIES(
  ${PROJECT_NAME}
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
  $<INSTALL_INTERFACE:${INSTALL_INCLUDEDIR}> )

TARGET_LINK_LIBRARIES(
  ${PROJECT_NAME}
  PUBLIC
  ${LAPACK_LIBRARIES}
  ${BLAS_LIBRARIES}
  "$<$<BOOL:${OpenMP_FOUND}>:${OpenMP_Fortran_LIBRARIES}>"
)

FOREACH(p PENF BeFoR64 FACE StringiFor)
  FIND_PACKAGE( ${p} REQUIRED )
  IF( ${p}_FOUND )
  GET_TARGET_PROPERTY(
    ${p}_INCLUDEDIR
    ${p}::${p}
    INTERFACE_INCLUDE_DIRECTORIES)
  MESSAGE(STATUS "${p} found")

  TARGET_LINK_LIBRARIES(
    ${PROJECT_NAME}
    PUBLIC
    ${p}::${p} )

  TARGET_INCLUDE_DIRECTORIES(
    ${PROJECT_NAME}
    PUBLIC
    "${${p}_INCLUDEDIR}" )
ELSE()
  MESSAGE(STATUS "${p} not found")
ENDIF()
ENDFOREACH()

INSTALL(
  TARGETS
    ${PROJECT_NAME}
  EXPORT
    ${TARGETS_EXPORT_NAME}
  ARCHIVE
    DESTINATION ${INSTALL_LIBDIR}
    COMPONENT lib
  RUNTIME
    DESTINATION ${INSTALL_BINDIR}
    COMPONENT bin
  LIBRARY
    DESTINATION ${INSTALL_LIBDIR}
    COMPONENT lib
)

install(
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    DESTINATION "./"
    COMPONENT lib
)

install(
  EXPORT
    ${TARGETS_EXPORT_NAME}
    NAMESPACE ${namespace}::
    DESTINATION ${INSTALL_CMAKEDIR}
    COMPONENT dev
)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION ${INSTALL_CMAKEDIR}
)
if(BUILD_SHARED_LIBS OR PROJECT_VERSION_MAJOR EQUAL 0)
  # Due to the uncertain ABI compatibility of Fortran shared libraries
  # limit compatibility for dynamic linking to same minor version.
  set(COMPATIBILITY AnyNewerVersion)
  # set(COMPATIBILITY SameMinorVersion)
else()
  # Require API compatibility via semantic versioning for static linking.
  set(COMPATIBILITY AnyNewerVersion)
  # set(COMPATIBILITY SameMajorVersion)
endif()

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ${COMPATIBILITY}
)

install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  DESTINATION
    ${INSTALL_CMAKEDIR}
)
