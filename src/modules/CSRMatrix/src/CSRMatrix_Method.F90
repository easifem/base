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
INTEGER( I4B ), PARAMETER, PUBLIC :: SPARSE_FMT_CSR = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: SPARSE_FMT_COO = 1
!!
#include "./ConstructorMethods.inc"
#include "./IOMethods.inc"
#include "./SparsityMethods.inc"
#include "./SetMethods.inc"
#include "./AddMethods.inc"
#include "./SetRowMethods.inc"
#include "./SetColMethods.inc"
#include "./SetBlockRowMethods.inc"
#include "./SetBlockColMethods.inc"
#include "./GetMethods.inc"

#include "./GetRowMethods.inc"
#include "./GetColMethods.inc"
#include "./GetBlockRowMethods.inc"
#include "./GetBlockColMethods.inc"

#include "./UnaryMethods.inc"
#include "./ILUTMethods.inc"
#include "./LUSolveMethods.inc"
#include "./MatVecMethods.inc"
#include "./MatmulMethods.inc"
#include "./ReorderingMethods.inc"

#include "./DiagonalScalingMethods.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE CSRMatrix_Method
