# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------
set(pkg_name "libsparsekit")
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

list(APPEND "${pkg_name}_SOURCES"
  ${src_path}apl.f
  ${src_path}blassm.f
  ${src_path}ccn.f
  ${src_path}color.f
  ${src_path}dinfo13.f
  ${src_path}dsepart.f
  ${src_path}formats.f
  ${src_path}ilut.f
  ${src_path}infofun.f
  ${src_path}inout.f
  ${src_path}iters.f
  ${src_path}matvec.f
  ${src_path}unary.f
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

target_link_libraries(
  ${pkg_name} PUBLIC
  ${LAPACK_LIBRARIES}
  ${BLAS_LIBRARIES}
)

target_compile_options(
  ${pkg_name}
  PRIVATE
  "-O3" "-std=legacy"
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
