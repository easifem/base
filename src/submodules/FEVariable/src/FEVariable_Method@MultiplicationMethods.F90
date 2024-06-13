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
#include "./include/ScalarOperatorScalar.F90"
  !! scalar, vector
  CASE (vector)
#include "./include/ScalarOperatorVector.F90"
  !! scalar, matrix
  CASE (matrix)
#include "./include/ScalarOperatorMatrix.F90"
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
#include "./include/VectorOperatorScalar.F90"
  !! vector, vector
  CASE (vector)
#include "./include/VectorOperatorVector.F90"
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
#include "./include/MatrixOperatorScalar.F90"
  CASE (matrix)
    !! matrix, matrix
#include "./include/MatrixOperatorMatrix.F90"
  END SELECT
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
#include "./include/ScalarOperatorReal.F90"
!!
!!
!!
!!
CASE (VECTOR)
#include "./include/VectorOperatorReal.F90"
!!
!!
!!
!!
CASE (MATRIX)
#include "./include/MatrixOperatorReal.F90"
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
#include "./include/RealOperatorScalar.F90"
!!
!!
!!
!!
CASE (VECTOR)
#include "./include/RealOperatorVector.F90"
!!
!!
!!
!!
CASE (MATRIX)
#include "./include/RealOperatorMatrix.F90"
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
