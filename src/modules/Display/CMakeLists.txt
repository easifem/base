# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

target_sources(
  ${PROJECT_NAME} PRIVATE
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