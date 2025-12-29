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

SUBMODULE(FEVariable_MultiplicationMethod) Methods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix, Nodal, Quadrature

USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableTime
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: varopt => TypeFEVariableOpt

USE ReallocateUtility, ONLY: Reallocate

USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, Get

#define _OP_ *

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication1
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :), m2(:, :)
INTEGER(I4B) :: jj, kk

SELECT CASE (obj1%rank)

CASE (varopt%scalar)

  SELECT CASE (obj2%rank)

    ! scalar * scalar
  CASE (varopt%scalar)

#include "./include/ScalarOperatorScalar.F90"

  CASE (varopt%vector)

#include "./include/ScalarOperatorVector.F90"

  CASE (varopt%matrix)

#include "./include/ScalarOperatorMatrix.F90"

  END SELECT

CASE (varopt%vector)

  SELECT CASE (obj2%rank)

  CASE (varopt%scalar)

#include "./include/VectorOperatorScalar.F90"

  CASE (varopt%vector)

#include "./include/VectorOperatorVector.F90"

  END SELECT

CASE (matrix)

  SELECT CASE (obj2%rank)

  CASE (varopt%scalar)

#include "./include/MatrixOperatorScalar.F90"

  CASE (varopt%matrix)

#include "./include/MatrixOperatorMatrix.F90"

  END SELECT

END SELECT
END PROCEDURE fevar_Multiplication1

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication_1
! INTEGER(I4B) :: rankCase
!
! rankCase = GetRankCase(obj1%rank, obj2%rank)
!
! SELECT CASE (rankCase)
!
! CASE (00)
!   CALL scalar_scalar_master(obj1, obj2, ans)
!
! CASE (01)
!   CALL scalar_vector_master(obj1, obj2, ans)
!
! CASE (02)
!   CALL scalar_matrix_master(obj1, obj2, ans)
!
! CASE (10)
!   CALL vector_scalar_master(obj1, obj2, ans)
!
! CASE (11)
!   CALL vector_vector_master(obj1, obj2, ans)
!
! CASE (20)
!   CALL matrix_scalar_master(obj1, obj2, ans)
!
! CASE (22)
!   CALL matrix_matrix_master(obj1, obj2, ans)
!
! END SELECT

END PROCEDURE fevar_Multiplication_1

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication2
SELECT CASE (obj1%rank)
CASE (scalar)
#include "./include/ScalarOperatorReal.F90"
CASE (vector)
#include "./include/VectorOperatorReal.F90"
CASE (matrix)
#include "./include/MatrixOperatorReal.F90"
END SELECT
END PROCEDURE fevar_Multiplication2

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication3
SELECT CASE (obj1%rank)
CASE (scalar)
#include "./include/RealOperatorScalar.F90"
CASE (vector)
#include "./include/RealOperatorVector.F90"
CASE (matrix)
#include "./include/RealOperatorMatrix.F90"
END SELECT
END PROCEDURE fevar_Multiplication3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#undef _OP_

END SUBMODULE Methods
