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
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
!!
INTEGER(I4B), PARAMETER, PUBLIC :: SPARSE_FMT_CSR = 0
INTEGER(I4B), PARAMETER, PUBLIC :: SPARSE_FMT_COO = 1
!!
#include "./include/ConstructorMethods.inc"
#include "./include/IOMethods.inc"
#include "./include/SparsityMethods.inc"
#include "./include/SetMethods.inc"
#include "./include/AddMethods.inc"
#include "./include/SetRowMethods.inc"
#include "./include/SetColMethods.inc"
#include "./include/SetBlockRowMethods.inc"
#include "./include/SetBlockColMethods.inc"
#include "./include/GetMethods.inc"

#include "./include/GetRowMethods.inc"
#include "./include/GetColMethods.inc"
#include "./include/GetBlockRowMethods.inc"
#include "./include/GetBlockColMethods.inc"

#include "./include/UnaryMethods.inc"
#include "./include/ILUTMethods.inc"
#include "./include/LUSolveMethods.inc"
#include "./include/MatVecMethods.inc"
#include "./include/MatmulMethods.inc"
#include "./include/ReorderingMethods.inc"

#include "./include/DiagonalScalingMethods.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_Method
