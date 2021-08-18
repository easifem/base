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
  ${src_path}/CSRMatrix_Method@Constructor.f90
  ${src_path}/CSRMatrix_Method@setMethod.f90
  ${src_path}/CSRMatrix_Method@getMethod.f90
  ${src_path}/CSRMatrix_Method@IO.f90
  ${src_path}/CSRMatrix_Method@Unary.f90
  ${src_path}/CSRMatrix_Method@ILUTMethods.f90
  ${src_path}/CSRMatrix_Method@LUsolveMethods.f90
  ${src_path}/CSRMatrix_Method@MatvecMethods.f90
  ${src_path}/CSRMatrix_Method@ReorderingMethod.f90 )