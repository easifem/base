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

SUBMODULE(ElemshapeData_GradientMethods) Methods
USE BaseMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_1
IF (obj%nsd .EQ. obj%xidim) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, obj%nsd, obj%nips)
END IF
END PROCEDURE elemsd_GetSpatialGradient_1

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_2
IF (obj%nsd .EQ. obj%xidim) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, SIZE(val, 1), obj%nsd, obj%nips)
END IF
END PROCEDURE elemsd_GetSpatialGradient_2

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_3
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%nsd .EQ. obj%xidim) THEN
    lg = Contraction(val, obj%dNTdXt)
  ELSE
    CALL Reallocate(lg, obj%nsd, obj%nips)
  END IF
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_3

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_4
INTEGER(I4B) :: ii, jj, ips
REAL(DFP), ALLOCATABLE :: r3(:, :, :)

CALL Reallocate(lg, SIZE(val, 1), obj%nsd, obj%nips)

SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%nsd .EQ. obj%xidim) THEN
    CALL SWAP(a=r3, b=val, i1=2, i2=3, i3=1)
    DO ips = 1, SIZE(lg, 3)
      DO jj = 1, SIZE(lg, 2)
        DO ii = 1, SIZE(lg, 1)
          lg(ii, jj, ips) = contraction(a1=r3(:, :, ii), &
                                        a2=obj%dNTdXt(:, :, jj, ips))
        END DO
      END DO
    END DO
    DEALLOCATE (r3)
  END IF
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_4

!----------------------------------------------------------------------------
!                                                           GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_5
SELECT CASE (val%varType)
CASE (constant)
  CALL Reallocate(lg, obj%nsd, obj%nips)
CASE (space)
  CALL GetSpatialGradient(obj=obj, lg=lg, &
    & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL GetSpatialGradient(obj=obj, lg=lg, &
      & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_5

!----------------------------------------------------------------------------
!                                                           GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_6
INTEGER(I4B) :: s(1)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL Reallocate(lg, s(1), obj%nsd, obj%nips)
CASE (space)
  CALL GetSpatialGradient(obj=obj, lg=lg, &
    & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL GetSpatialGradient(obj=obj, lg=lg, &
      & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_6

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_7
IF (obj%nsd .EQ. obj%xidim) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, SIZE(val, 1), SIZE(val, 2), obj%nsd, obj%nips)
END IF
END PROCEDURE elemsd_GetSpatialGradient_7

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_8
INTEGER(I4B) :: ii, jj
  !!
CALL Reallocate(lg, SIZE(val, 1), SIZE(val, 2), obj%nsd, obj%nips)
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%nsd .EQ. obj%xidim) THEN
    DO jj = 1, SIZE(lg, 4)
      DO ii = 1, SIZE(lg, 3)
        lg(:, :, ii, jj) = contraction(a1=val, &
          & a2=obj%dNTdXt(:, :, ii, jj))
      END DO
    END DO
  END IF
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_8

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_9
INTEGER(I4B) :: s(2)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL Reallocate(lg, s(1), s(2), obj%nsd, obj%nips)
CASE (space)
  CALL GetSpatialGradient(obj=obj, lg=lg, &
    & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL GetSpatialGradient(obj=obj, lg=lg, &
      & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_9

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_10
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :)
  !!
SELECT CASE (val%rank)
CASE (scalar)
  CALL GetSpatialGradient(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
CASE (vector)
  CALL GetSpatialGradient(obj=obj, lg=r3, val=val)
  lg = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
  DEALLOCATE (r3)
CASE (matrix)
    !! BUG Implement gradient of matrix
    !! TODO Extend FEVariable to support r3, which is necessary to keep
    !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_10

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSpatialGradient_11
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
INTEGER(I4B) :: ii
  !!
SELECT CASE (val%rank)
  !!
  !! scalar
  !!
CASE (scalar)
  DO ii = 1, SIZE(obj)
    CALL GetSpatialGradient(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
    END IF
      !!
    r3(:, :, ii) = r2(:, :)
  END DO
  lg = QuadratureVariable(r3, typeFEVariableVector,&
    & typeFEVariableSpaceTime)
  DEALLOCATE (r2, r3)
  !!
  !! vector
  !!
CASE (vector)
  DO ii = 1, SIZE(obj)
    CALL GetSpatialGradient(obj=obj(ii), lg=r3, val=val)
    IF (.NOT. ALLOCATED(r4)) THEN
      CALL Reallocate(r4, SIZE(r3, 1), SIZE(r3, 2), SIZE(r3, 3), SIZE(obj))
    END IF
      !!
    r4(:, :, :, ii) = r3(:, :, :)
  END DO
  lg = QuadratureVariable(r4, typeFEVariableMatrix, &
                          typeFEVariableSpaceTime)
  DEALLOCATE (r3, r4)
  !!
  !! matrix TODO
  !!
CASE (matrix)
    !! BUG Implement gradient of matrix
    !! TODO Extend FEVariable to support r3, which is necessary to keep
    !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_GetSpatialGradient_11

!----------------------------------------------------------------------------
!                                                            SpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SpatialGradient_1
CALL GetSpatialGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_SpatialGradient_1

!----------------------------------------------------------------------------
!                                                            SpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SpatialGradient_2
CALL GetSpatialGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_SpatialGradient_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
