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

SUBMODULE(ElemshapeData_Method) interpolMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE elemsd_getinterpolation_1

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_2
SELECT CASE (val%vartype)
CASE (Constant)
  CALL Reallocate(interpol, SIZE(obj%N, 2))
  interpol = Get(val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol = interpolation(obj, &
      & Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
  ELSE
    interpol = Get(val, TypeFEVariableScalar, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (val%DefineOn .EQ. Nodal) THEN
      interpol = STinterpolation(obj, &
        & Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE elemsd_getinterpolation_2

!---------------------------------------------------------------------------
!                                                          getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_3
interpol = MATMUL(val, obj%N)
END PROCEDURE elemsd_getinterpolation_3

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_4
interpol = MATMUL(val, obj%N)
END PROCEDURE elemsd_getinterpolation_4

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_5
INTEGER(I4B) :: i
INTEGER(I4B), ALLOCATABLE :: s(:)
!! main
SELECT CASE (val%vartype)
CASE (Constant)
  s = SHAPE(val)
  CALL reallocate(interpol, s(1), s(2), SIZE(obj%N, 2))
  interpol(:, :, 1) = Get(val, TypeFEVariableMatrix, &
    & TypeFEVariableConstant)
  DO i = 2, SIZE(interpol, 3)
    interpol(:, :, i) = interpol(:, :, 1)
  END DO
  DEALLOCATE (s)
CASE (Space)
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol = interpolation(obj, &
      & Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
  ELSE
    interpol = Get(val, TypeFEVariableMatrix, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (val%DefineOn .EQ. Nodal) THEN
      interpol = STinterpolation(obj, &
        & Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE elemsd_getinterpolation_5

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_6
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE elemsd_getinterpolation_6

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_7
INTEGER(I4B) :: ii
!! main
CALL Reallocate(interpol, SIZE(obj(1)%N, 2), SIZE(obj))
SELECT CASE (val%vartype)
CASE (Constant)
  interpol = Get(val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol(:, 1) = interpolation(obj(1), &
        & Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
  ELSE
    interpol(:, 1) = Get(val, TypeFEVariableScalar,&
      & TypeFEVariableSpace)
  END IF
  DO ii = 2, SIZE(obj)
    interpol(:, ii) = interpol(:, 1)
  END DO
CASE (SpaceTime)
  IF (val%DefineOn .EQ. Nodal) THEN
    DO ii = 1, SIZE(obj)
      interpol(:, ii) = STinterpolation(obj(ii), &
        & Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END DO
  ELSE
    interpol = Get(val, TypeFEVariableScalar, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE elemsd_getinterpolation_7

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_8
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE elemsd_getinterpolation_8

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_9
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE elemsd_getinterpolation_9

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_10
INTEGER(I4B) :: ii, jj
!! main
SELECT CASE (val%vartype)
CASE (Constant)
  interpol(:, :, 1, 1) = Get(val, TypeFEVariableMatrix,&
    & TypeFEVariableConstant)
  DO jj = 1, SIZE(interpol, 4)
    DO ii = 1, SIZE(interpol, 3)
      IF (ii .EQ. 1 .AND. jj .EQ. 1) CYCLE
      interpol(:, :, ii, jj) = interpol(:, :, 1, 1)
    END DO
  END DO
CASE (Space)
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol(:, :, :, 1) = interpolation(obj(1), &
        & Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
    DO ii = 2, SIZE(interpol, 4)
      interpol(:, :, :, ii) = interpol(:, :, :, 1)
    END DO
  ELSE
    interpol(:, :, :, 1) = Get(val, TypeFEVariableMatrix,&
      & TypeFEVariableSpace)
    DO ii = 2, SIZE(interpol, 4)
      interpol(:, :, :, ii) = interpol(:, :, :, 1)
    END DO
  END IF
CASE (SpaceTime)
  IF (val%DefineOn .EQ. Nodal) THEN
    DO ii = 1, SIZE(interpol, 4)
      interpol(:, :, :, ii) = STinterpolation(obj(ii), &
        & Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END DO
  ELSE
    interpol = Get(val, TypeFEVariableMatrix, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE elemsd_getinterpolation_10

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getinterpolation_11
REAL(DFP), ALLOCATABLE :: realvec(:)
INTEGER(I4B) :: ii
!! main
SELECT CASE (val%vartype)
CASE (Constant)
  realvec = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
  CALL Reallocate(interpol, SIZE(realvec), SIZE(obj%N, 2))
  DO ii = 1, SIZE(interpol, 2)
    interpol(:, ii) = realvec
  END DO
  DEALLOCATE (realvec)
CASE (Space)
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol = interpolation(obj, &
      & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
  ELSE
    interpol = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (val%DefineOn .EQ. Nodal) THEN
      interpol = STinterpolation(obj, &
        & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE elemsd_getinterpolation_11

!----------------------------------------------------------------------------
!                                                              interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_scalar
interpol = MATMUL(val, obj%N)
END PROCEDURE interpol_scalar

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_vector
interpol = MATMUL(val, obj%N)
END PROCEDURE interpol_vector

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_matrix
interpol = MATMUL(val, obj%N)
END PROCEDURE interpol_matrix
!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_scalar
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE stsd_interpol_scalar

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_vector
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE stsd_interpol_vector

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_matrix
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE stsd_interpol_matrix

END SUBMODULE interpolMethods
