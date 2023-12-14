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
  PRIVATE ${src_path}/CSRMatrix_AddMethods.F90
          ${src_path}/CSRMatrix_ConstructorMethods.F90
          ${src_path}/CSRMatrix_DiagonalScalingMethods.F90
          ${src_path}/CSRMatrix_GetBlockColMethods.F90
          ${src_path}/CSRMatrix_GetBlockRowMethods.F90
          ${src_path}/CSRMatrix_GetColMethods.F90
          ${src_path}/CSRMatrix_GetMethods.F90
          ${src_path}/CSRMatrix_GetRowMethods.F90
          ${src_path}/CSRMatrix_GetSubMatrixMethods.F90
          ${src_path}/CSRMatrix_ILUMethods.F90
          ${src_path}/CSRMatrix_IOMethods.F90
          ${src_path}/CSRMatrix_LUSolveMethods.F90
          ${src_path}/CSRMatrix_MatmulMethods.F90
          ${src_path}/CSRMatrix_MatVecMethods.F90
          ${src_path}/CSRMatrix_ReorderingMethods.F90
          ${src_path}/CSRMatrix_SetBlockColMethods.F90
          ${src_path}/CSRMatrix_SetBlockRowMethods.F90
          ${src_path}/CSRMatrix_SetColMethods.F90
          ${src_path}/CSRMatrix_SetMethods.F90
          ${src_path}/CSRMatrix_SetRowMethods.F90
          ${src_path}/CSRMatrix_SparsityMethods.F90
          ${src_path}/CSRMatrix_UnaryMethods.F90
          ${src_path}/CSRMatrix_Method.F90
          ${src_path}/CSRMatrix_SpectralMethods.F90
          ${src_path}/CSRMatrix_MatrixMarketIO.F90
          ${src_path}/CSRMatrix_DBCMethods.F90
          ${src_path}/CSRMatrix_LinSolveMethods.F90)

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(${PROJECT_NAME} PRIVATE ${src_path}/CSRMatrix_SuperLU.F90
                                       ${src_path}/CSRMatrix_SchurMethods.F90)
