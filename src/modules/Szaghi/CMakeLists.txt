# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

list(APPEND szaghi_SOURCES
  PENF/penf_global_parameters_variables.f90
  PENF/penf_b_size.f90
  PENF/penf_stringify.f90
  PENF/penf.f90
  BeFoR64/befor64_pack_data_m.f90
  BeFoR64/befor64.f90
  StringiFor/stringifor_string_t.f90
  StringiFor/stringifor.f90
)

add_library(szaghi_obj OBJECT "")
target_sources(szaghi_obj PRIVATE
  "${szaghi_SOURCES}"
)
target_include_directories(
  szaghi_obj PRIVATE
  ${EASIFEM_BASE_INCLUDE}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}
)
target_compile_options(
  szaghi_obj
  PRIVATE
  ${FORTRAN_FLAGS_RELEASE}
)

if(BUILD_SHARED_LIBS)
  add_library(libszaghi SHARED $<TARGET_OBJECTS:szaghi_obj>)
else()
  add_library(libszaghi STATIC $<TARGET_OBJECTS:szaghi_obj>)
endif()
install(TARGETS libszaghi
  DESTINATION ${EASIFEM_BASE_LIB})
