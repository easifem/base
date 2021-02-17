# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------
set(pkg_name ${PROJECT_NAME})
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

list(APPEND "${pkg_name}_SOURCES"
  ${src_path}disp_i1mod.f90
  ${src_path}disp_i2mod.f90
  ${src_path}disp_i4mod.f90
  ${src_path}disp_i8mod.f90
  ${src_path}disp_l1mod.f90
  ${src_path}disp_r16mod.f90
  ${src_path}dispmodule_util.f90
  ${src_path}dispmodule.f90
  ${src_path}putstrmodule.f90
  ${src_path}/Display_Method.f90
)

target_sources(
  ${pkg_name} PRIVATE
  "${${pkg_name}_SOURCES}"
)

target_include_directories(
  ${pkg_name} PUBLIC
  ${EASIFEM_BASE_INCLUDE}
  ${CMAKE_SOURCE_DIR}
)