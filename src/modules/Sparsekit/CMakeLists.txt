# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------
set(pkg_name "libsparsekit")
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

list(APPEND _SOURCES
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

set_source_files_properties(
  ${_SOURCES}
  PROPERTIES
  COMPILE_OPTIONS ${FORTRAN_FLAGS_SPARSEKIT}
)

target_sources(
  ${PROJECT_NAME} PRIVATE
  "${_SOURCES}"
)

target_link_libraries(
  ${PROJECT_NAME} PUBLIC
  ${LAPACK_LIBRARIES}
  ${BLAS_LIBRARIES}
)