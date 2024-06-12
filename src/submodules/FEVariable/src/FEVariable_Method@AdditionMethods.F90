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
#define _OP_ +

SUBMODULE(FEVariable_Method) AdditionMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_addition1
!!
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :), m2(:, :)
INTEGER(I4B) :: jj, kk
!!
SELECT CASE (obj1%rank)
!!
!!
!!
!!
CASE (SCALAR)
  !!
  SELECT CASE (obj2%rank)
  !! scalar, scalar
  CASE (scalar)
#include "./ScalarOperatorScalar.inc"
  !! scalar, vector
  CASE (vector)
#include "./ScalarOperatorVector.inc"
  !! scalar, matrix
  CASE (matrix)
#include "./ScalarOperatorMatrix.inc"
  END SELECT
!!
!!
!!
!!
CASE (VECTOR)
  !!
  SELECT CASE (obj2%rank)
  !! vector, scalar
  CASE (scalar)
#include "./VectorOperatorScalar.inc"
  !! vector, vector
  CASE (vector)
#include "./VectorOperatorVector.inc"
  END SELECT
!!
!!
!!
!!
CASE (MATRIX)
  !!
  SELECT CASE (obj2%rank)
  CASE (scalar)
    !! matrix, scalar
#include "./MatrixOperatorScalar.inc"
  CASE (matrix)
    !! matrix, matrix
#include "./MatrixOperatorMatrix.inc"
  END SELECT
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE fevar_addition1

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_addition2
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
END PROCEDURE fevar_addition2

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_addition3
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
END PROCEDURE fevar_addition3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AdditionMethods
#undef _OP_
