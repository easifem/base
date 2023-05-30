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

SUBMODULE(ElemshapeData_LocalDivergenceMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_1
lg = Contraction(a1=TRANSPOSE(val), a2=obj%dNdXi)
END PROCEDURE elemsd_getLocalDivergence_1

!----------------------------------------------------------------------------
!                                                        getLocalDivergence
!----------------------------------------------------------------------------z

MODULE PROCEDURE elemsd_getLocalDivergence_2
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = Contraction(a1=TRANSPOSE(MATMUL(Val, obj%T)), &
      & a2=obj%dNdXi)
END SELECT
END PROCEDURE elemsd_getLocalDivergence_2

!----------------------------------------------------------------------------
!                                                           getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_3
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(lg, SIZE(obj%N, 2))
CASE (space)
  CALL getLocalDivergence(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalDivergence(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getLocalDivergence_3

!----------------------------------------------------------------------------
!                                                         getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_4
INTEGER(I4B) :: ii, n
n = SIZE(obj%N, 2)
CALL reallocate(lg, SIZE(val, 1), n)
DO ii = 1, n
  lg(:, ii) = contraction(val, TRANSPOSE(obj%dNdXi(:, :, ii)))
END DO
END PROCEDURE elemsd_getLocalDivergence_4

!----------------------------------------------------------------------------
!                                                           getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_5
INTEGER(I4B) :: ii, n
REAL(DFP), ALLOCATABLE :: r3(:, :, :)
!!
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  n = SIZE(obj%N, 2)
  CALL reallocate(lg, SIZE(val, 1), n)
  r3 = MATMUL(val, obj%T)
  DO ii = 1, n
    lg(:, ii) = contraction(r3, TRANSPOSE(obj%dNdXi(:, :, ii)))
  END DO
END SELECT
!!
IF (ALLOCATED(r3)) DEALLOCATE (r3)
END PROCEDURE elemsd_getLocalDivergence_5

!----------------------------------------------------------------------------
!                                                         getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_6
INTEGER(I4B) :: s(2)
SELECT CASE (val%varType)
CASE (constant)
  s = SHAPE(val)
  CALL reallocate(lg, s(1), SIZE(obj%N, 2))
CASE (space)
  CALL getLocalDivergence(obj=obj, lg=lg, &
       & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalDivergence(obj=obj, lg=lg, &
         & Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_getLocalDivergence_6

!----------------------------------------------------------------------------
!                                                         getLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_7
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)
!!
SELECT CASE (val%rank)
CASE (vector)
  CALL getLocalDivergence(obj=obj, lg=r1, val=val)
  lg = QuadratureVariable(r1, typeFEVariableScalar, typeFEVariableSpace)
  DEALLOCATE (r1)
CASE (matrix)
  CALL getLocalDivergence(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
END SELECT
END PROCEDURE elemsd_getLocalDivergence_7

!----------------------------------------------------------------------------
!                                                           LocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getLocalDivergence_8
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
INTEGER(I4B) :: ii
!!
SELECT CASE (val%rank)
!!
!! vector
!!
CASE (vector)
  DO ii = 1, SIZE(obj)
    CALL getLocalDivergence(obj=obj(ii), lg=r1, val=val)
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
    CALL getLocalDivergence(obj=obj(ii), lg=r2, val=val)
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
END PROCEDURE elemsd_getLocalDivergence_8

!----------------------------------------------------------------------------
!                                                            LocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalDivergence_1
CALL getLocalDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalDivergence_1

!----------------------------------------------------------------------------
!                                                            LocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalDivergence_2
CALL getLocalDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalDivergence_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
