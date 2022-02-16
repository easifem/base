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

SUBMODULE(ElemshapeData_Method) GradientMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_1
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, obj%refelem%nsd, SIZE(obj%N, 2))
END IF
END PROCEDURE elemsd_getSpatialGradient_1

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_2
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, SIZE(val, 1), obj%refelem%nsd, &
    & SIZE(obj%N, 2))
END IF
END PROCEDURE elemsd_getSpatialGradient_2

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_3
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
    lg = Contraction(val, obj%dNTdXt)
  ELSE
    CALL Reallocate(lg, obj%refelem%nsd, SIZE(obj%N, 2))
  END IF
END SELECT
END PROCEDURE elemsd_getSpatialGradient_3

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_4
INTEGER(I4B) :: ii, jj, ips
REAL(DFP), ALLOCATABLE :: r3(:, :, :)
!!
CALL Reallocate(lg, SIZE(val, 1), obj%refelem%nsd, &
  & SIZE(obj%N, 2))
!!
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
    CALL SWAP(a=r3, b=val, i1=2, i2=3, i3=1)
    DO ips = 1, SIZE(lg, 3)
      DO jj = 1, SIZE(lg, 2)
        DO ii = 1, SIZE(lg, 1)
          lg(ii, jj, ips) = contraction(a1=r3(:, :, ii), &
            & a2=obj%dNTdXt(:, :, jj, ips))
        END DO
      END DO
    END DO
    DEALLOCATE (r3)
  END IF
END SELECT
END PROCEDURE elemsd_getSpatialGradient_4

!----------------------------------------------------------------------------
!                                                           getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_5
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(lg, obj%refelem%nsd, SIZE(obj%N, 2))
CASE (space)
  CALL getSpatialGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL getSpatialGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getSpatialGradient_5

!----------------------------------------------------------------------------
!                                                           getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_6
INTEGER(I4B) :: s(1)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), obj%refelem%nsd, &
       & SIZE(obj%N, 2))
CASE (space)
  CALL getSpatialGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getSpatialGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getSpatialGradient_6

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_7
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  lg = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(lg, SIZE(val, 1), SIZE(val, 2), &
       & obj%refelem%nsd, SIZE(obj%N, 2))
END IF
END PROCEDURE elemsd_getSpatialGradient_7

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_8
INTEGER(I4B) :: ii, jj
!!
CALL Reallocate(lg, SIZE(val, 1), SIZE(val, 2), obj%refelem%nsd, &
  & SIZE(obj%N, 2))
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
    DO jj = 1, SIZE(lg, 4)
      DO ii = 1, SIZE(lg, 3)
        lg(:, :, ii, jj) = contraction(a1=val, &
             & a2=obj%dNTdXt(:, :, ii, jj))
      END DO
    END DO
  END IF
END SELECT
END PROCEDURE elemsd_getSpatialGradient_8

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_9
INTEGER(I4B) :: s(2)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), s(2),  &
       & obj%refelem%nsd, SIZE(obj%N, 2))
CASE (space)
  CALL getSpatialGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getSpatialGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getSpatialGradient_9

!----------------------------------------------------------------------------
!                                                        getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_10
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :)
!!
SELECT CASE (val%rank)
CASE (scalar)
  CALL getSpatialGradient(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
CASE (vector)
  CALL getSpatialGradient(obj=obj, lg=r3, val=val)
  lg = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
  DEALLOCATE (r3)
CASE (matrix)
   !! BUG Implement gradient of matrix
   !! TODO Extend FEVariable to support r3, which is necessary to keep
   !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_getSpatialGradient_10

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSpatialGradient_11
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
INTEGER(I4B) :: ii
!!
SELECT CASE (val%rank)
!!
!! scalar
!!
CASE (scalar)
  DO ii = 1, SIZE(obj)
    CALL getSpatialGradient(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
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
    CALL getSpatialGradient(obj=obj(ii), lg=r3, val=val)
    IF (.NOT. ALLOCATED(r4)) THEN
      CALL reallocate(r4, SIZE(r3, 1), SIZE(r3, 2), SIZE(r3, 3), SIZE(obj))
    END IF
     !!
    r4(:, :, :, ii) = r3(:, :, :)
  END DO
  lg = QuadratureVariable(r4, typeFEVariableMatrix,&
       & typeFEVariableSpaceTime)
  DEALLOCATE (r3, r4)
!!
!! matrix TODO
!!
CASE (matrix)
   !! BUG Implement gradient of matrix
   !! TODO Extend FEVariable to support r3, which is necessary to keep
   !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_getSpatialGradient_11

!----------------------------------------------------------------------------
!                                                            SpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SpatialGradient_1
CALL getSpatialGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_SpatialGradient_1

!----------------------------------------------------------------------------
!                                                            SpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SpatialGradient_2
CALL getSpatialGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_SpatialGradient_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GradientMethods
