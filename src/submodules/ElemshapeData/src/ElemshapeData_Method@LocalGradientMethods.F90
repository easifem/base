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

SUBMODULE(ElemshapeData_Method) LocalGradientMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_1
lg = MATMUL(Val, obj%dNdXi)
!! matmul r1 r3
END PROCEDURE elemsd_getLocalGradient_1

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_2
lg = MATMUL(Val, obj%dNdXi)
!! matmul r2 r3
END PROCEDURE elemsd_getLocalGradient_2

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_3
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
  !! matmul r1 r3
END SELECT
END PROCEDURE elemsd_getLocalGradient_3

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_4
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
  !! (r3.r1).r3 => r2.r3
END SELECT
END PROCEDURE elemsd_getLocalGradient_4

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_5
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(lg, obj%refelem%xidimension, SIZE(obj%N, 2))
CASE (space)
  CALL getLocalGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getLocalGradient_5

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_6
INTEGER(I4B) :: s(1)
!!
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), obj%refelem%xidimension, SIZE(obj%N, 2))
CASE (space)
  CALL getLocalGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getLocalGradient_6

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_7
lg = MATMUL(val, obj%dNdXi)
!! r3.r4
END PROCEDURE elemsd_getLocalGradient_7

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_8
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  lg = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
  !! (r4.r1).r3
END SELECT
END PROCEDURE elemsd_getLocalGradient_8

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_9
INTEGER(I4B) :: s(2)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), s(2),  &
       & obj%refelem%xidimension, SIZE(obj%N, 2))
CASE (space)
  CALL getLocalGradient(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalGradient(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getLocalGradient_9

!----------------------------------------------------------------------------
!                                                         getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_10
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :)
!!
SELECT CASE (val%rank)
CASE (scalar)
  CALL getLocalGradient(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
CASE (vector)
  CALL getLocalGradient(obj=obj, lg=r3, val=val)
  lg = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
  DEALLOCATE (r3)
CASE (matrix)
   !! BUG Implement gradient of matrix
   !! TODO Extend FEVariable to support r3, which is necessary to keep
   !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_getLocalGradient_10

!----------------------------------------------------------------------------
!                                                         getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalGradient_11
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
INTEGER(I4B) :: ii
!!
SELECT CASE (val%rank)
!!
!! scalar
!!
CASE (scalar)
  DO ii = 1, SIZE(obj)
    CALL getLocalGradient(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
    END IF
     !!
    r3(:, :, ii) = r2(:, :)
  END DO
  lg = QuadratureVariable(r3, typeFEVariableVector,&
       & typeFEVariableSpaceTime)
  DEALLOCATE (r2,r3)
!!
!! vector
!!
CASE (vector)
  DO ii = 1, SIZE(obj)
    CALL getLocalGradient(obj=obj(ii), lg=r3, val=val)
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
END PROCEDURE elemsd_getLocalGradient_11

!----------------------------------------------------------------------------
!                                                              LocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalGradient_1
CALL getLocalGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalGradient_1

!----------------------------------------------------------------------------
!                                                              LocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalGradient_2
CALL getLocalGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalGradient_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LocalGradientMethods
