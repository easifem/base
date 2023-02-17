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

SUBMODULE(ElemshapeData_InterpolMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_getinterpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE scalar_getinterpolation_1

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_getinterpolation_2
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END SELECT
END PROCEDURE scalar_getinterpolation_2

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_getinterpolation_3
INTEGER(I4B) :: ipt
CALL reallocate(interpol, SIZE(obj(1)%N, 2), SIZE(obj))
DO ipt = 1, SIZE(obj)
  interpol(:, ipt) = MATMUL(MATMUL(val, obj(ipt)%T), obj(ipt)%N)
END DO
END PROCEDURE scalar_getinterpolation_3

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_getinterpolation_4
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
END PROCEDURE scalar_getinterpolation_4

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_getinterpolation_5
INTEGER(I4B) :: ii
! REAL(DFP), ALLOCATABLE :: m1(:)
! !! main
! CALL Reallocate(interpol, SIZE(obj(1)%N, 2), SIZE(obj))
! DO ii = 1, SIZE(obj)
!   CALL getInterpolation(obj=obj(ii), interpol=m1, val=val)
!   interpol(:, ii) = m1
! END DO
! DEALLOCATE (m1)
CALL Reallocate(interpol, SIZE(obj(1)%N, 2), SIZE(obj))
!!
SELECT CASE (val%vartype)
!!
!!
!!
!!
CASE (Constant)
  !!
  interpol = Get(val, TypeFEVariableScalar, TypeFEVariableConstant)
!!
!!
!!
!!
CASE (Space)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, ii) = Interpolation(obj(ii), &
        & Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
    END DO
    !!
  ELSE
    !!
    interpol(:, 1) = Get(val, TypeFEVariableScalar, TypeFEVariableSpace)
    !!
    DO ii = 2, SIZE(obj)
      interpol(:, ii) = interpol(:, 1)
    END DO
    !!
  END IF
!!
!!
!!
!!
CASE (SpaceTime)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, ii) = STinterpolation(obj(ii), &
        & Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END DO
    !!
  ELSE
    interpol = Get(val, TypeFEVariableScalar, typeFEVariableSpaceTime)
  END IF
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE scalar_getinterpolation_5

!---------------------------------------------------------------------------
!                                                          getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE vector_getinterpolation_1

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_2
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END SELECT
END PROCEDURE vector_getinterpolation_2

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_3
INTEGER(I4B) :: ipt
!!
CALL reallocate(interpol, SIZE(val, 1), SIZE(obj(1)%N, 2), SIZE(obj))
DO ipt = 1, SIZE(obj)
  interpol(:, :, ipt) = MATMUL(MATMUL(val, obj(ipt)%T), obj(ipt)%N)
END DO
END PROCEDURE vector_getinterpolation_3

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_4
REAL(DFP), ALLOCATABLE :: m1(:)
INTEGER(I4B) :: ii
!! main
SELECT CASE (val%vartype)
!!
!! Constant
!!
CASE (Constant)
  !!
  m1 = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
  CALL Reallocate(interpol, SIZE(m1), SIZE(obj%N, 2))
  DO ii = 1, SIZE(interpol, 2)
    interpol(:, ii) = m1
  END DO
  DEALLOCATE (m1)
!!
!! Space
!!
CASE (Space)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    interpol = interpolation(obj, &
      & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
  ELSE
    interpol = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
  END IF
!!
!! SpaceTime
!!
CASE (SpaceTime)
  !!
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    interpol = STinterpolation(obj, &
      & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
!!
!!
!!
END PROCEDURE vector_getinterpolation_4

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_5
! INTEGER(I4B) :: ii
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: m2(:, :)
! !! main
! s = SHAPE(val)
! CALL Reallocate(interpol, s(1), SIZE(obj(1)%N, 2), SIZE(obj))
! DO ii = 1, SIZE(obj)
!   CALL getInterpolation(obj=obj(ii), interpol=m2, val=val)
!   interpol(:, :, ii) = m2
! END DO
! DEALLOCATE (m2, s)
!!
REAL(DFP), ALLOCATABLE :: m1(:)
INTEGER(I4B) :: ii, jj
INTEGER(I4B), ALLOCATABLE :: s(:)
!!
!! main
!!
s = SHAPE(val)
CALL Reallocate(interpol, s(1), SIZE(obj(1)%N, 2), SIZE(obj))
!!
SELECT CASE (val%vartype)
!!
!! Constant
!!
CASE (Constant)
  !!
  m1 = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
  !!
  DO jj = 1, SIZE(interpol, 3)
    DO ii = 1, SIZE(interpol, 2)
      interpol(:, ii, jj) = m1
    END DO
  END DO
  DEALLOCATE (m1)
!!
!! Space
!!
CASE (Space)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, :, ii) = Interpolation(obj(ii), &
        & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
    END DO
    !!
  ELSE
    !!
    interpol(:, :, 1) = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
    !!
    DO ii = 2, SIZE(obj)
      interpol(:, :, ii) = interpol(:, :, 1)
    END DO
    !!
  END IF
!!
!! SpaceTime
!!
CASE (SpaceTime)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, :, ii) = STinterpolation(obj(ii), &
        & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
    END DO
    !!
  ELSE
    interpol = Get(val, TypeFEVariableVector, typeFEVariableSpaceTime)
  END IF
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE vector_getinterpolation_5

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE matrix_getinterpolation_1

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_2
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END SELECT
END PROCEDURE matrix_getinterpolation_2

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_3
!! TODO
END PROCEDURE matrix_getinterpolation_3

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_4
INTEGER(I4B) :: i
INTEGER(I4B) :: s(2)
!! main
SELECT CASE (val%vartype)
CASE (Constant)
  s(1:2) = SHAPE(val)
  CALL reallocate(interpol, s(1), s(2), SIZE(obj%N, 2))
  interpol(:, :, 1) = Get(val, TypeFEVariableMatrix, &
    & TypeFEVariableConstant)
  DO i = 2, SIZE(interpol, 3)
    interpol(:, :, i) = interpol(:, :, 1)
  END DO
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
END PROCEDURE matrix_getinterpolation_4

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_5
! INTEGER(I4B) :: ii
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: m3(:, :, :)
! !! main
! s = SHAPE(val)
! CALL Reallocate(interpol, s(1), s(2), SIZE(obj(1)%N, 2), SIZE(obj))
! DO ii = 1, SIZE(obj)
!   CALL getInterpolation(obj=obj(ii), interpol=m3, val=val)
!   interpol(:, :, :, ii) = m3
! END DO
! DEALLOCATE (m3, s)
!!
INTEGER(I4B) :: ii, jj
INTEGER(I4B), ALLOCATABLE :: s(:)
REAL(DFP), ALLOCATABLE :: m2(:, :)
!!
!! main
!!
s = SHAPE(val)
CALL Reallocate(interpol, s(1), s(2), SIZE(obj(1)%N, 2), SIZE(obj))
!!
SELECT CASE (val%vartype)
!!
!!
!!
!!
CASE (Constant)
  !!
  m2 = Get(val, TypeFEVariableMatrix, TypeFEVariableConstant)
  !!
  DO jj = 1, SIZE(interpol, 4)
    DO ii = 1, SIZE(interpol, 3)
      interpol(:, :, ii, jj) = m2
    END DO
  END DO
  !!
  DEALLOCATE (m2)
!!
!!
!!
!!
CASE (Space)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, :, :, ii) = Interpolation(obj(ii), &
        & Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
    END DO
    !!
  ELSE
    !!
    interpol(:, :, :, 1) = Get(val, TypeFEVariableMatrix, TypeFEVariableSpace)
    !!
    DO ii = 2, SIZE(obj)
      interpol(:, :, :, ii) = interpol(:, :, :, 1)
    END DO
    !!
  END IF
!!
!!
!!
!!
CASE (SpaceTime)
  !!
  IF (val%DefineOn .EQ. Nodal) THEN
    !!
    DO ii = 1, SIZE(obj)
      interpol(:, :, :, ii) = STinterpolation(obj(ii), &
        & Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END DO
    !!
  ELSE
    interpol = Get(val, TypeFEVariableMatrix, typeFEVariableSpaceTime)
  END IF
!!
!!
!!
!!
END SELECT
!!
END PROCEDURE matrix_getinterpolation_5

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE master_getinterpolation_1
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
!! main
!!
!! if val is a quadrature variable then do nothing
!!
IF (val%defineOn .EQ. Quadrature) THEN
  interpol = val
  RETURN
END IF
!!
!! if val is a nodal variable then interpolate
!!
SELECT CASE (val%rank)
CASE (Scalar)
  CALL getInterpolation(obj=obj, interpol=r1, val=val)
  interpol = QuadratureVariable(r1, typeFEVariableScalar, &
    & typeFEVariableSpace)
  DEALLOCATE (r1)
CASE (Vector)
  CALL getInterpolation(obj=obj, interpol=r2, val=val)
  interpol = QuadratureVariable(r2, typeFEVariableVector, &
    & typeFEVariableSpace)
  DEALLOCATE (r2)
CASE (Matrix)
  CALL getInterpolation(obj=obj, interpol=r3, val=val)
  interpol = QuadratureVariable(r3, typeFEVariableMatrix, &
    & typeFEVariableSpace)
  DEALLOCATE (r3)
END SELECT

END PROCEDURE master_getinterpolation_1

!----------------------------------------------------------------------------
!                                                         getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE master_getInterpolation_2
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
!! main
!!
!! if val is a quadrature variable then do nothing
!!
IF (val%defineOn .EQ. Quadrature) THEN
  interpol = val
  RETURN
END IF
!!
!! if val is a nodal variable then interpolate
!!
SELECT CASE (val%rank)
CASE (Scalar)
  CALL getInterpolation(obj=obj, interpol=r2, val=val)
  interpol = QuadratureVariable(r2, typeFEVariableScalar, &
    & typeFEVariableSpaceTime)
  DEALLOCATE (r2)
CASE (Vector)
  CALL getInterpolation(obj=obj, interpol=r3, val=val)
  interpol = QuadratureVariable(r3, typeFEVariableVector, &
    & typeFEVariableSpaceTime)
  DEALLOCATE (r3)
CASE (Matrix)
  CALL getInterpolation(obj=obj, interpol=r4, val=val)
  interpol = QuadratureVariable(r4, typeFEVariableMatrix, &
    & typeFEVariableSpaceTime)
  DEALLOCATE (r4)
END SELECT
!!
END PROCEDURE master_getInterpolation_2

!----------------------------------------------------------------------------
!                                                              interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_interpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE scalar_interpolation_1

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_interpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE vector_interpolation_1

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_interpolation_1
interpol = MATMUL(val, obj%N)
END PROCEDURE matrix_interpolation_1

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE master_interpolation_1
CALL getInterpolation(obj=obj, val=val, interpol=ans)
END PROCEDURE master_interpolation_1

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_stinterpolation_1
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE scalar_stinterpolation_1

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_stinterpolation_1
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE vector_stinterpolation_1

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_stinterpolation_1
interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE matrix_stinterpolation_1

END SUBMODULE Methods
