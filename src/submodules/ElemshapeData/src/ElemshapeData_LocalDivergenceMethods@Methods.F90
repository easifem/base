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
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableOpt
USE ContractionUtility, ONLY: Contraction
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE MatmulUtility, ONLY: MATMUL
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_1
lg = Contraction(a1=TRANSPOSE(val), a2=obj%dNdXi)
END PROCEDURE elemsd_GetLocalDivergence_1

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_2
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = Contraction(a1=TRANSPOSE(MATMUL(Val, obj%T)), &
                   a2=obj%dNdXi)
END SELECT
END PROCEDURE elemsd_GetLocalDivergence_2

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_3
SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  CALL reallocate(lg, SIZE(obj%N, 2))
CASE (TypeFEVariableOpt%space)
  CALL GetLocalDivergence(obj=obj, lg=lg, &
                          val=Get(val, TypeFEVariableVector, &
                                  TypeFEVariableSpace))
CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL GetLocalDivergence(obj=obj, lg=lg, &
                            val=Get(val, TypeFEVariableVector, &
                                    TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetLocalDivergence_3

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_4
INTEGER(I4B) :: ii, n
n = SIZE(obj%N, 2)
CALL Reallocate(lg, SIZE(val, 1), n)
DO ii = 1, n
  lg(:, ii) = Contraction(val, TRANSPOSE(obj%dNdXi(:, :, ii)))
END DO
END PROCEDURE elemsd_GetLocalDivergence_4

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_5
INTEGER(I4B) :: ii, n
REAL(DFP), ALLOCATABLE :: r3(:, :, :)

SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  n = SIZE(obj%N, 2)
  CALL reallocate(lg, SIZE(val, 1), n)
  r3 = MATMUL(val, obj%T)
  DO ii = 1, n
    lg(:, ii) = contraction(r3, TRANSPOSE(obj%dNdXi(:, :, ii)))
  END DO
END SELECT

IF (ALLOCATED(r3)) DEALLOCATE (r3)
END PROCEDURE elemsd_GetLocalDivergence_5

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_6
INTEGER(I4B) :: s(2)
SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  s = SHAPE(val)
  CALL Reallocate(lg, s(1), SIZE(obj%N, 2))
CASE (TypeFEVariableOpt%space)
  CALL GetLocalDivergence(obj=obj, lg=lg, &
                          val=Get(val, TypeFEVariableMatrix, &
                                  TypeFEVariableSpace))
CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL GetLocalDivergence(obj=obj, lg=lg, &
                            val=Get(val, TypeFEVariableMatrix, &
                                    TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetLocalDivergence_6

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_7
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)

SELECT CASE (val%RANK)
CASE (TypeFEVariableOpt%vector)
  CALL GetLocalDivergence(obj=obj, lg=r1, val=val)
  lg = QuadratureVariable(r1, typeFEVariableScalar, typeFEVariableSpace)
  DEALLOCATE (r1)
CASE (TypeFEVariableOpt%matrix)
  CALL GetLocalDivergence(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)
END SELECT
END PROCEDURE elemsd_GetLocalDivergence_7

!----------------------------------------------------------------------------
!                                                          GetLocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalDivergence_8
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
INTEGER(I4B) :: ii

SELECT CASE (val%RANK)

!! vector
CASE (TypeFEVariableOpt%vector)
  DO ii = 1, SIZE(obj)
    CALL GetLocalDivergence(obj=obj(ii), lg=r1, val=val)
    IF (.NOT. ALLOCATED(r1)) RETURN
    IF (.NOT. ALLOCATED(r2)) THEN
      CALL reallocate(r2, SIZE(r1, 1), SIZE(obj))
    END IF

    r2(:, ii) = r1
  END DO
  IF (ALLOCATED(r2)) THEN
    lg = QuadratureVariable(r2, typeFEVariableScalar, &
                            typeFEVariableSpaceTime)
  END IF
  IF (ALLOCATED(r1)) DEALLOCATE (r1)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)

!! matrix
CASE (TypeFEVariableOpt%matrix)
  DO ii = 1, SIZE(obj)
    CALL GetLocalDivergence(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r2)) RETURN
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
    END IF

    r3(:, :, ii) = r2
  END DO
  IF (ALLOCATED(r3)) THEN
    lg = QuadratureVariable(r3, typeFEVariableVector, &
                            typeFEVariableSpaceTime)
  END IF
  IF (ALLOCATED(r2)) DEALLOCATE (r2)
  IF (ALLOCATED(r3)) DEALLOCATE (r3)
END SELECT
END PROCEDURE elemsd_GetLocalDivergence_8

!----------------------------------------------------------------------------
!                                                             LocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalDivergence_1
CALL GetLocalDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalDivergence_1

!----------------------------------------------------------------------------
!                                                             LocalDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalDivergence_2
CALL GetLocalDivergence(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalDivergence_2

END SUBMODULE Methods
