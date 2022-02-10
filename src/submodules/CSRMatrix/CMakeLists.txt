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
  ${src_path}/CSRMatrix_Method@ConstructorMethods.F90
  ${src_path}/CSRMatrix_Method@IOMethods.F90
  ${src_path}/CSRMatrix_Method@SparsityMethods.F90
  ${src_path}/CSRMatrix_Method@SetMethods.F90
  ${src_path}/CSRMatrix_Method@AddMethods.F90
  ${src_path}/CSRMatrix_Method@SetRowMethods.F90
  ${src_path}/CSRMatrix_Method@SetColMethods.F90
  ${src_path}/CSRMatrix_Method@SetBlockRowMethods.F90
  ${src_path}/CSRMatrix_Method@SetBlockColMethods.F90
  ${src_path}/CSRMatrix_Method@GetMethods.F90
  ${src_path}/CSRMatrix_Method@GetRowMethods.F90
  ${src_path}/CSRMatrix_Method@GetColMethods.F90
  ${src_path}/CSRMatrix_Method@GetBlockRowMethods.F90
  ${src_path}/CSRMatrix_Method@GetBlockColMethods.F90
  ${src_path}/CSRMatrix_Method@UnaryMethods.F90
  ${src_path}/CSRMatrix_Method@ILUTMethods.F90
  ${src_path}/CSRMatrix_Method@LUSolveMethods.F90
  ${src_path}/CSRMatrix_Method@MatVecMethods.F90
  # ${src_path}/CSRMatrix_Method@MatmulMethods.F90
  ${src_path}/CSRMatrix_Method@ReorderingMethod.F90
  ${src_path}/CSRMatrix_Method@DiagonalScalingMethods.F90
)