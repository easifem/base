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

SUBMODULE(ElemshapeData_Method) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_1
! Define internal variables
INTEGER(I4B) :: i
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO i = 1, SIZE(obj%N, 2)
  cdNdXt(:, i) = MATMUL(obj%dNdXt(:, :, i), Val)
END DO
END PROCEDURE elemsd_GetProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: CBar(:, :)
!! main
!! Compute CBar from Val
SELECT CASE (Val%VarType)
CASE (Constant)
  CALL Reallocate(CBar, obj%refelem%nsd, SIZE(obj%N, 2))
  CBar(:, 1) = Get(Val, TypeVariableVector, TypeVariableConstant)
  DO ii = 2, SIZE(CBar, 2)
    CBar(:, ii) = CBar(:, 1)
  END DO
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    CALL getInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpace))
  ELSE
    CBar = Get(Val, TypeVariableVector, TypeVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL getInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpaceTime))
  END SELECT
END SELECT
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), CBar(:, ii))
END DO
IF (ALLOCATED(CBar)) DEALLOCATE (CBar)
END PROCEDURE elemsd_GetProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_1
INTEGER(I4B) :: ii
CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), Val)
END DO
END PROCEDURE getProjectionOfdNTdXt_1

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: CBar(:, :)
!! main
SELECT CASE (Val%VarType)
!! constant variable type
CASE (Constant)
  CALL Reallocate(CBar, obj%refelem%nsd, SIZE(obj%N, 2))
  CBar(:, 1) = Get(Val, TypeVariableVector, TypeVariableConstant)
  DO ii = 2, SIZE(CBar, 2)
    CBar(ii, :) = CBar(:, 1)
  END DO
!! spatial variable
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    CALL GetInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpace))
  ELSE
    CBar = Get(Val, TypeVariableVector, TypeVariableSpace)
  END IF
!! space-time variable
CASE (SpaceTime)
  CALL getInterpolation(obj=obj, Interpol=CBar, &
       & Val=Get(Val, TypeVariableVector, TypeVariableSpaceTime))
END SELECT
!!
CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), CBar(:, ii))
END DO
DEALLOCATE (CBar)
END PROCEDURE getProjectionOfdNTdXt_2

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_1
#include "./getUnitNormal_1.inc"
END PROCEDURE getUnitNormal_1

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_2
#include "./getUnitNormal_2.inc"
END PROCEDURE getUnitNormal_2

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_3
IF (val%rank .EQ. scalar) THEN
  CALL scalar_getUnitNormal_3(obj=obj, r=r, val=val)
ELSEIF (val%rank .EQ. vector) THEN
  CALL vector_getUnitNormal_3(obj=obj, r=r, val=val)
END IF
END PROCEDURE getUnitNormal_3

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE scalar_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_1.inc"
END SUBROUTINE scalar_getUnitNormal_3

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE vector_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_2.inc"
END SUBROUTINE vector_getUnitNormal_3

END SUBMODULE getMethod
