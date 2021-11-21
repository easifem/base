# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 4/03/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/FEVariable_Method@ConstructorMethods.F90
  ${src_path}/FEVariable_Method@IOMethods.F90
  ${src_path}/FEVariable_Method@GetMethods.F90
)
