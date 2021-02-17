# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(pkg_name "libszaghi")
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

list(APPEND "${pkg_name}_SOURCES"
  ${src_path}PENF/penf_global_parameters_variables.f90
  ${src_path}PENF/penf_b_size.f90
  ${src_path}PENF/penf_stringify.f90
  ${src_path}PENF/penf.f90
  ${src_path}BeFoR64/befor64_pack_data_m.f90
  ${src_path}BeFoR64/befor64.f90
  ${src_path}StringiFor/stringifor_string_t.f90
  ${src_path}StringiFor/stringifor.f90
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
  ${CMAKE_SOURCE_DIR}
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
