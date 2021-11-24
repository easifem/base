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

SUBMODULE(ElemshapeData_Method) InterpolMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_1
Interpol = MATMUL(Val, obj%N)
END PROCEDURE elemsd_getInterpolation_1

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_2
SELECT CASE (Val%VarType)
CASE (Constant)
  CALL Reallocate(Interpol, SIZE(obj%N, 2))
  Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    Interpol = Interpolation(obj, &
      & Get(Val, TypeFEVariableScalar, TypeFEVariableSpace))
  ELSE
    Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (Val%DefineOn .EQ. Nodal) THEN
      Interpol = STInterpolation(obj, &
        & Get(Val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE elemsd_getInterpolation_2

!---------------------------------------------------------------------------
!                                                          getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_3
Interpol = MATMUL(Val, obj%N)
END PROCEDURE elemsd_getInterpolation_3

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_4
Interpol = MATMUL(Val, obj%N)
END PROCEDURE elemsd_getInterpolation_4

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_5
INTEGER(I4B) :: i
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol(:, :, 1) = Get(Val, TypeFEVariableMatrix, &
    & TypeFEVariableConstant)
  DO i = 2, SIZE(obj%N, 2)
    Interpol(:, :, i) = Interpol(:, :, 1)
  END DO
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    Interpol = Interpolation(obj, &
      & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace))
  ELSE
    Interpol = Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (Val%DefineOn .EQ. Nodal) THEN
      Interpol = STInterpolation(obj, &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE elemsd_getInterpolation_5

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_6
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE elemsd_getInterpolation_6

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_7
INTEGER(I4B) :: ii
!! main
CALL Reallocate(interpol, SIZE(obj(1)%N, 2), SIZE(obj))
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    Interpol(:, 1) = Interpolation(obj(1), &
        & Get(Val, TypeFEVariableScalar, TypeFEVariableSpace))
    DO ii = 2, SIZE(obj)
      Interpol(:, ii) = Interpol(:, 1)
    END DO
  ELSE
    Interpol(:, 1) = Get(Val, TypeFEVariableScalar,&
      & TypeFEVariableSpace)
    DO ii = 2, SIZE(obj)
      Interpol(:, ii) = Interpol(:, 1)
    END DO
  END IF
CASE (SpaceTime)
  IF (Val%DefineOn .EQ. Nodal) THEN
    DO ii = 1, SIZE(obj)
      Interpol(:, ii) = STInterpolation(obj(ii), &
        & Get(Val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END DO
  ELSE
    Interpol = Get(Val, TypeFEVariableScalar, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE elemsd_getInterpolation_7

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_8
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE elemsd_getInterpolation_8

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_9
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE elemsd_getInterpolation_9

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getInterpolation_10
INTEGER(I4B) :: ii, jj
!! main
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol(:, :, 1, 1) = Get(Val, TypeFEVariableMatrix,&
    & TypeFEVariableConstant)
  DO jj = 1, SIZE(Interpol, 4)
    DO ii = 1, SIZE(Interpol, 3)
      IF (ii .EQ. 1 .AND. jj .EQ. 1) CYCLE
      Interpol(:, :, ii, jj) = Interpol(:, :, 1, 1)
    END DO
  END DO
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    Interpol(:, :, :, 1) = Interpolation(obj(1), &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace))
    DO ii = 2, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = Interpol(:, :, :, 1)
    END DO
  ELSE
    Interpol(:, :, :, 1) = Get(Val, TypeFEVariableMatrix,&
      & TypeFEVariableSpace)
    DO ii = 2, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = Interpol(:, :, :, 1)
    END DO
  END IF
CASE (SpaceTime)
  IF (Val%DefineOn .EQ. Nodal) THEN
    DO ii = 1, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = STInterpolation(obj(ii), &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END DO
  ELSE
    Interpol = Get(Val, TypeFEVariableMatrix, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE elemsd_getInterpolation_10

!----------------------------------------------------------------------------
!                                                              Interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_scalar
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_scalar

!----------------------------------------------------------------------------
!                                                      InterpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_vector
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_vector

!----------------------------------------------------------------------------
!                                                      InterpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_matrix
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_matrix
!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_scalar
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_scalar

!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_vector
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_vector

!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_matrix
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_matrix

END SUBMODULE InterpolMethods
