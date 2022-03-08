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
#define _OP_ *

SUBMODULE(FEVariable_Method) MultiplicationMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication1
!!
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:,:,:,:), m2(:,:)
INTEGER(I4B) :: jj, kk
!!
SELECT CASE (obj1%rank)
!!
!!
!!
!!
CASE (SCALAR)
  !!
  select case( obj2%rank )
  !! scalar, scalar
  case( scalar )
#include "./ScalarOperatorScalar.inc"
  !! scalar, vector
  case( vector )
#include "./ScalarOperatorVector.inc"
  !! scalar, matrix
  case( matrix )
#include "./ScalarOperatorMatrix.inc"
  end select
!!
!!
!!
!!
CASE (VECTOR)
  !!
  select case( obj2%rank )
  !! vector, scalar
  case( scalar )
#include "./VectorOperatorScalar.inc"
  !! vector, vector
  case( vector )
#include "./VectorOperatorVector.inc"
  end select
!!
!!
!!
!!
CASE (MATRIX)
  !!
  select case( obj2%rank )
  case( scalar )
    !! matrix, scalar
#include "./MatrixOperatorScalar.inc"
  case( matrix )
    !! matrix, matrix
#include "./MatrixOperatorMatrix.inc"
  end select
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE fevar_Multiplication1

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication2
SELECT CASE (obj1%rank)
!!
!!
!!
!!
CASE (SCALAR)
#include "./ScalarOperatorReal.inc"
!!
!!
!!
!!
CASE (VECTOR)
#include "./VectorOperatorReal.inc"
!!
!!
!!
!!
CASE (MATRIX)
#include "./MatrixOperatorReal.inc"
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE fevar_Multiplication2

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication3
SELECT CASE (obj1%rank)
!!
!!
!!
!!
CASE (SCALAR)
#include "./RealOperatorScalar.inc"
!!
!!
!!
!!
CASE (VECTOR)
#include "./RealOperatorVector.inc"
!!
!!
!!
!!
CASE (MATRIX)
#include "./RealOperatorMatrix.inc"
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE fevar_Multiplication3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MultiplicationMethods
#undef _OP_
