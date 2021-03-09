# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 4/03/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/RealMatrix_Method@IO.f90
  ${src_path}/RealMatrix_Method@Constructor.f90
  ${src_path}/RealMatrix_Method@getValues.f90
  ${src_path}/RealMatrix_Method@setValues.f90
  ${src_path}/RealMatrix_Method@MatrixMultiplication.f90
  ${src_path}/RealMatrix_Method@LAPACK.f90
)
