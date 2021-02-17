# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

list(APPEND sparsekit_SOURCES
  apl.f
  blassm.f
  ccn.f
  color.f
  dinfo13.f
  dsepart.f
  formats.f
  ilut.f
  infofun.f
  inout.f
  iters.f
  matvec.f
  unary.f
)
add_library(sparsekit_obj OBJECT "")
target_sources(sparsekit_obj PRIVATE
  "${sparsekit_SOURCES}"
)
target_include_directories(
  sparsekit_obj PRIVATE
  ${EASIFEM_BASE_INCLUDE}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}
)
target_compile_options(
  sparsekit_obj
  PRIVATE
  "-O3" "-std=legacy"
)

if(BUILD_SHARED_LIBS)
  add_library(libsparsekit SHARED $<TARGET_OBJECTS:sparsekit_obj>)
  target_link_libraries(libsparsekit PUBLIC
  ${LAPACK_LIBRARIES}
  ${BLAS_LIBRARIES})
else()
  add_library(libsparsekit STATIC $<TARGET_OBJECTS:sparsekit_obj>)
endif()
install(TARGETS libsparsekit
  DESTINATION ${EASIFEM_BASE_LIB})
