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

SUBMODULE(ElemshapeData_Method) DivergenceMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_1
lg = Contraction(a1=TRANSPOSE(val), a2=obj%dNdXt)
END PROCEDURE elemsd_getDivergence_1

!----------------------------------------------------------------------------
!                                                             getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_2
REAL(DFP), ALLOCATABLE :: r3(:,:,:)
!! main
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  CALL SWAP(a=r3, b=val, i1=2, i2=3, i3=1)
  lg = Contraction(r3, obj%dNTdXt)
  DEALLOCATE(r3)
END SELECT
END PROCEDURE elemsd_getDivergence_2

!----------------------------------------------------------------------------
!                                                              getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_3
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(lg, SIZE(obj%N, 2))
CASE (space)
  CALL getDivergence(obj=obj, lg=lg, &
      & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getDivergence(obj=obj, lg=lg, &
        & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getDivergence_3

!----------------------------------------------------------------------------
!                                                              getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_4
INTEGER(I4B) :: ii, n
!!
n = SIZE(obj%N, 2)
CALL reallocate(lg, SIZE(val, 1), n)
DO ii = 1, n
  lg(:, ii) = contraction(val, TRANSPOSE(obj%dNdXt(:, :, ii)))
END DO
END PROCEDURE elemsd_getDivergence_4

!----------------------------------------------------------------------------
!                                                              getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_5
REAL(DFP), ALLOCATABLE :: r4(:, :, :, :)
INTEGER( I4B ) :: ii
!!
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  CALL SWAP(a=r4, b=val, i1=3, i2=4, i3=2, i4=1)
  CALL Reallocate(lg, size(obj%N, 2), size(val, 1))
  DO ii = 1, SIZE(r4, 4)
    lg(:, ii) = Contraction(a1=r4(:,:,:,ii), a2=obj%dNTdXt)
  END DO
  lg = TRANSPOSE(lg)
  Deallocate(r4)
END SELECT
!!
END PROCEDURE elemsd_getDivergence_5

!----------------------------------------------------------------------------
!                                                              getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_6
INTEGER(I4B) :: s(2)
!!
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), SIZE(obj%N, 2))
CASE (space)
  CALL getDivergence(obj=obj, lg=lg, &
      & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getDivergence(obj=obj, lg=lg, &
        & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getDivergence_6

!----------------------------------------------------------------------------
!                                                              getDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_7
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)
!!
SELECT CASE (val%rank)
CASE (vector)
  CALL getDivergence(obj=obj, lg=r1, val=val)
  lg = QuadratureVariable(r1, typeFEVariableScalar, typeFEVariableSpace)
  DEALLOCATE (r1)
CASE (matrix)
  CALL getDivergence(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
END SELECT
END PROCEDURE elemsd_getDivergence_7

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getDivergence_8
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
INTEGER(I4B) :: ii
!!
SELECT CASE (val%rank)
!!
!! vector
!!
CASE (vector)
  DO ii = 1, SIZE(obj)
    CALL getDivergence(obj=obj(ii), lg=r1, val=val)
    IF (.NOT. ALLOCATED(r2)) THEN
      CALL reallocate(r2, SIZE(r1, 1), SIZE(obj))
    END IF
    !!
    r2(:, ii) = r1
  END DO
  lg = QuadratureVariable(r2, typeFEVariableScalar,&
      & typeFEVariableSpaceTime)
  DEALLOCATE (r2, r1)
!!
!! matrix
!!
CASE (matrix)
  DO ii = 1, SIZE(obj)
    CALL getDivergence(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
    END IF
    !!
    r3(:, :, ii) = r2
  END DO
  lg = QuadratureVariable(r3, typeFEVariableVector,&
      & typeFEVariableSpaceTime)
  DEALLOCATE (r2, r3)
END SELECT
END PROCEDURE elemsd_getDivergence_8

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Divergence_1
CALL getDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_Divergence_1

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Divergence_2
CALL getDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_Divergence_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DivergenceMethods
