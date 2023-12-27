! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE CSRMatrix_Method
USE CSRMatrix_ConstructorMethods
USE CSRMatrix_IOMethods
USE CSRMatrix_SparsityMethods
USE CSRMatrix_SetMethods
USE CSRMatrix_AddMethods
USE CSRMatrix_SetRowMethods
USE CSRMatrix_SetColMethods
USE CSRMatrix_SetBlockRowMethods
USE CSRMatrix_SetBlockColMethods
USE CSRMatrix_GetMethods
USE CSRMatrix_GetRowMethods
USE CSRMatrix_GetColMethods
USE CSRMatrix_GetSubMatrixMethods
USE CSRMatrix_GetBlockRowMethods
USE CSRMatrix_GetBlockColMethods
USE CSRMatrix_UnaryMethods
USE CSRMatrix_ILUMethods
USE CSRMatrix_LUSolveMethods
USE CSRMatrix_MatVecMethods
USE CSRMatrix_MatmulMethods
USE CSRMatrix_ReorderingMethods
USE CSRMatrix_DiagonalScalingMethods
USE CSRMatrix_MatrixMarketIO
USE CSRMatrix_Superlu
USE CSRMatrix_SpectralMethods
USE CSRMatrix_SchurMethods
USE CSRMatrix_DBCMethods
USE CSRMatrix_LinSolveMethods
USE CSRMatrix_SymMatmulMethods
USE GlobalData, ONLY: I4B
IMPLICIT NONE
INTEGER(I4B), PARAMETER, PUBLIC :: SPARSE_FMT_CSR = 0
INTEGER(I4B), PARAMETER, PUBLIC :: SPARSE_FMT_COO = 1
END MODULE CSRMatrix_Method
