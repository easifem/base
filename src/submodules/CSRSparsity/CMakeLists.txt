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
  ${src_path}/CSRSparsity_Method@ConstructorMethods.F90
  ${src_path}/CSRSparsity_Method@IOMethods.F90
  ${src_path}/CSRSparsity_Method@setMethods.F90
)