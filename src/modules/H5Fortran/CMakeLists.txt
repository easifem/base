# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

set(pkg_name "libh5fortran")
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

list(APPEND "${pkg_name}_SOURCES"
  ${src_path}interface.f90
  ${src_path}read.f90
  ${src_path}reader.f90
  ${src_path}reader_lt.f90
  ${src_path}string_utils.f90
  ${src_path}write.f90
  ${src_path}writer.f90
  ${src_path}writer_lt.f90
)

if(BUILD_SHARED_LIBS)
  add_library(${pkg_name} SHARED "")
else()
  add_library(${pkg_name} STATIC "")
endif()

target_sources(
  ${pkg_name} PRIVATE
  "${${pkg_name}_SOURCES}"
)

target_include_directories(
  ${pkg_name} PRIVATE
  ${EASIFEM_BASE_INCLUDE}
  ${HDF5_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}
)

target_link_libraries(
  ${pkg_name} PUBLIC
  ${HDF5_Fortran_LIBRARIES}
  ${HDF5_Fortran_HL_LIBRARIES}
)

target_compile_options(
  ${pkg_name}
  PRIVATE
  ${FORTRAN_FLAGS_RELEASE}
)

set_target_properties(
  ${pkg_name}
  PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${EASIFEM_BASE_LIB}"
  ARCHIVE_OUTPUT_DIRECTORY "${EASIFEM_BASE_LIB}"
  RUNTIME_OUTPUT_DIRECTORY "${EASIFEM_BASE_BIN}"
)

target_link_libraries(
  ${PROJECT_NAME} PUBLIC
  ${pkg_name}
)
