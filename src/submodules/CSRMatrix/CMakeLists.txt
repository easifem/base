# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/CSRMatrix_AddMethods@Methods.F90
          ${src_path}/CSRMatrix_ConstructorMethods@Methods.F90
          ${src_path}/CSRMatrix_DiagonalScalingMethods@Methods.F90
          ${src_path}/CSRMatrix_GetBlockColMethods@Methods.F90
          ${src_path}/CSRMatrix_GetColMethods@Methods.F90
          ${src_path}/CSRMatrix_GetMethods@Methods.F90
          ${src_path}/CSRMatrix_GetRowMethods@Methods.F90
          ${src_path}/CSRMatrix_GetSubMatrixMethods@Methods.F90
          ${src_path}/CSRMatrix_ILUMethods@Methods.F90
          ${src_path}/CSRMatrix_IOMethods@Methods.F90
          ${src_path}/CSRMatrix_LUSolveMethods@Methods.F90
          ${src_path}/CSRMatrix_MatVecMethods@Methods.F90
          ${src_path}/CSRMatrix_ReorderingMethods@Methods.F90
          ${src_path}/CSRMatrix_SetBlockColMethods@Methods.F90
          ${src_path}/CSRMatrix_SetBlockRowMethods@Methods.F90
          ${src_path}/CSRMatrix_SetColMethods@Methods.F90
          ${src_path}/CSRMatrix_SetRowMethods@Methods.F90
          ${src_path}/CSRMatrix_SetMethods@Methods.F90
          ${src_path}/CSRMatrix_SparsityMethods@Methods.F90
          ${src_path}/CSRMatrix_UnaryMethods@Methods.F90
          ${src_path}/CSRMatrix_SpectralMethods@Methods.F90
          ${src_path}/CSRMatrix_MatrixMarketIO@Methods.F90
          ${src_path}/CSRMatrix_DBCMethods@Methods.F90
          ${src_path}/CSRMatrix_LinSolveMethods@Methods.F90)

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE ${src_path}/CSRMatrix_SuperLU@Methods.F90
                          ${src_path}/CSRMatrix_SchurMethods@Methods.F90)
