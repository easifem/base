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

SUBMODULE(FEVariable_UnaryMethod) Methods
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
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

USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, Get
USE IntegerUtility, ONLY: Get1DIndexFortran
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                         Abs
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Abs
SELECT CASE (obj%rank)

#define _ELEM_METHOD_ ABS
CASE (scalar)
#include "./include/ScalarElemMethod.F90"

CASE (vector)
#include "./include/VectorElemMethod.F90"

CASE (matrix)
#include "./include/MatrixElemMethod.F90"

END SELECT
#undef _ELEM_METHOD_

END PROCEDURE fevar_Abs

!----------------------------------------------------------------------------
!                                                                      Power
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Power
SELECT CASE (obj%rank)
CASE (scalar)
#include "./include/ScalarPower.F90"
CASE (vector)
#include "./include/VectorPower.F90"
CASE (matrix)
#include "./include/MatrixPower.F90"
END SELECT
END PROCEDURE fevar_Power

!----------------------------------------------------------------------------
!                                                                       Sqrt
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Sqrt
#define _ELEM_METHOD_ SQRT

SELECT CASE (obj%rank)
CASE (scalar)
#include "./include/ScalarElemMethod.F90"
CASE (vector)
#include "./include/VectorElemMethod.F90"
CASE (matrix)
#include "./include/MatrixElemMethod.F90"
END SELECT

#define _ELEM_METHOD_ SQRT
END PROCEDURE fevar_Sqrt

!----------------------------------------------------------------------------
!                                                                    IsEqual
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_IsEqual
!! Internal variable
ans = .FALSE.
IF (obj1%len .NE. obj2%len) RETURN
IF (obj1%defineon .NE. obj2%defineon) RETURN
IF (obj1%rank .NE. obj2%rank) RETURN
IF (obj1%varType .NE. obj2%varType) RETURN
IF (ANY(obj1%s .NE. obj2%s)) RETURN

IF (ALL(obj1%val(1:obj1%len) .APPROXEQ.obj2%val(1:obj2%len))) ans = .TRUE.
!!
END PROCEDURE fevar_IsEqual

!----------------------------------------------------------------------------
!                                                                   NotEqual
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_NotEqual
ans = .FALSE.
IF (.NOT. ALL(obj1%val.APPROXEQ.obj2%val)) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%defineon .NE. obj2%defineon) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%rank .NE. obj2%rank) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%varType .NE. obj2%varType) THEN
  ans = .TRUE.
  RETURN
END IF

IF (ANY(obj1%s .NE. obj2%s)) THEN
  ans = .TRUE.
  RETURN
END IF
END PROCEDURE fevar_NotEqual

!----------------------------------------------------------------------------
!                                                             NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_norm2
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), m2(:, :), r3(:, :, :), m3(:, :, :)

INTEGER(I4B) :: jj, kk

SELECT CASE (obj%vartype)

CASE (constant)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(NORM2(obj%val(1:obj%len)), &
                        typeFEVariableScalar, typeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(NORM2(obj%val(1:obj%len)), &
                             typeFEVariableScalar, typeFEVariableConstant)
  END IF

CASE (space)

  r2 = GET(obj, TypeFEVariableVector, TypeFEVariableSpace)

  CALL Reallocate(r1, SIZE(r2, 2))

  DO jj = 1, SIZE(r1)
    r1(jj) = NORM2(r2(:, jj))
  END DO

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(r1, &
                        typeFEVariableScalar, typeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(r1, &
                             typeFEVariableScalar, typeFEVariableSpace)
  END IF

CASE (time)

  r2 = GET(obj, TypeFEVariableVector, TypeFEVariableTime)

  CALL Reallocate(r1, SIZE(r2, 2))

  DO jj = 1, SIZE(r1)
    r1(jj) = NORM2(r2(:, jj))
  END DO

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(r1, &
                        typeFEVariableScalar, typeFEVariableTime)
  ELSE
    ans = QuadratureVariable(r1, &
                             typeFEVariableScalar, typeFEVariableTime)
  END IF

CASE (spacetime)

  r3 = GET(obj, TypeFEVariableVector, TypeFEVariableSpaceTime)

  CALL Reallocate(r2, SIZE(r3, 2), SIZE(r3, 3))

  DO kk = 1, SIZE(r3, 3)
    DO jj = 1, SIZE(r3, 2)
      r2(jj, kk) = NORM2(r3(:, jj, kk))
    END DO
  END DO

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(r2, &
                        typeFEVariableScalar, typeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable(r2, &
                             typeFEVariableScalar, typeFEVariableSpaceTime)
  END IF

END SELECT
END PROCEDURE fevar_norm2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

