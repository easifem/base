# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 23/02/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/DOF_ConstructorMethods@Methods.F90
  ${src_path}/DOF_IOMethods@Methods.F90
  ${src_path}/DOF_SetMethods@Methods.F90
  ${src_path}/DOF_AddMethods@Methods.F90
  ${src_path}/DOF_GetMethods@Methods.F90
  ${src_path}/DOF_GetValueMethods@Methods.F90
)
