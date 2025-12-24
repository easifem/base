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

SUBMODULE(FEVariable_SubtractionMethod) Methods

USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix, &
                      Nodal, Quadrature
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    TypeFEVariableTime, &
                    TypeFEVariableSpaceTime

USE ReallocateUtility, ONLY: Reallocate

USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, Get

#define _OP_ -

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Subtraction
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Subtraction1
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :), m2(:, :)
INTEGER(I4B) :: jj, kk

SELECT CASE (obj1%rank)

CASE (scalar)

  SELECT CASE (obj2%rank)

  CASE (scalar)

#include "./include/ScalarOperatorScalar.F90"

  CASE (vector)

#include "./include/ScalarOperatorVector.F90"

  CASE (matrix)

#include "./include/ScalarOperatorMatrix.F90"
  END SELECT

CASE (vector)

  SELECT CASE (obj2%rank)

  CASE (scalar)

#include "./include/VectorOperatorScalar.F90"

  CASE (vector)

#include "./include/VectorOperatorVector.F90"
  END SELECT

CASE (matrix)

  SELECT CASE (obj2%rank)

  CASE (scalar)

#include "./include/MatrixOperatorScalar.F90"

  CASE (matrix)

#include "./include/MatrixOperatorMatrix.F90"
  END SELECT
END SELECT
END PROCEDURE fevar_Subtraction1

!----------------------------------------------------------------------------
!                                                                Subtraction
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Subtraction2

SELECT CASE (obj1%rank)

CASE (SCALAR)

#include "./include/ScalarOperatorReal.F90"

CASE (VECTOR)

#include "./include/VectorOperatorReal.F90"

CASE (MATRIX)

#include "./include/MatrixOperatorReal.F90"
END SELECT
END PROCEDURE fevar_Subtraction2

!----------------------------------------------------------------------------
!                                                                Subtraction
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Subtraction3

SELECT CASE (obj1%rank)

CASE (SCALAR)

#include "./include/RealOperatorScalar.F90"

CASE (VECTOR)

#include "./include/RealOperatorVector.F90"

CASE (MATRIX)

#include "./include/RealOperatorMatrix.F90"
END SELECT
END PROCEDURE fevar_Subtraction3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
#undef _OP_
