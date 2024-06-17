# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/disp/disp_i1mod.F90
  ${src_path}/disp/disp_i2mod.F90
  ${src_path}/disp/disp_i4mod.F90
  ${src_path}/disp/disp_i8mod.F90
  ${src_path}/disp/disp_l1mod.F90
  ${src_path}/disp/disp_r4mod.F90
  ${src_path}/disp/disp_r8mod.F90
  ${src_path}/disp/disp_r16mod.F90
  ${src_path}/disp/disp_charmod.F90
  ${src_path}/disp/dispmodule_util.F90
  ${src_path}/disp/dispmodule.F90
  ${src_path}/disp/putstrmodule.F90
  ${src_path}/Display_Method.F90
)