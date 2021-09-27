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
  ${src_path}/CSRMatrix_Method@Constructor.F90
  ${src_path}/CSRMatrix_Method@setMethod.F90
  ${src_path}/CSRMatrix_Method@getMethod.F90
  ${src_path}/CSRMatrix_Method@IO.F90
  ${src_path}/CSRMatrix_Method@Unary.F90
  ${src_path}/CSRMatrix_Method@ILUTMethods.F90
  ${src_path}/CSRMatrix_Method@LUsolveMethods.F90
  ${src_path}/CSRMatrix_Method@MatvecMethods.F90
  ${src_path}/CSRMatrix_Method@ReorderingMethod.F90 )