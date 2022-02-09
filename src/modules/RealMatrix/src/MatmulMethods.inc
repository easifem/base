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

PUBLIC :: Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realmat_MatMul1( obj1, obj2 ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj1, obj2
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realmat_MatMul1
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE realmat_MatMul1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realmat_MatMul2( obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  REAL( DFP ), ALLOCATABLE :: Ans( : )
END FUNCTION realmat_MatMul2
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE realmat_MatMul2
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realmat_MatMul3( obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  TYPE( RealVector_ ), INTENT( IN ) :: Vec
  TYPE( RealVector_ ) :: Ans
END FUNCTION realmat_MatMul3
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE realmat_MatMul3
END INTERFACE Matmul