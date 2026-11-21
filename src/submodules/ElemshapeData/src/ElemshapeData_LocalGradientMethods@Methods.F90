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

SUBMODULE(ElemshapeData_LocalGradientMethods) Methods
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableTime
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE FEVariable_Method, ONLY: FeVariableGet => Get
USE FEVariable_Method, ONLY: FeVariableShape => Shape
USE FEVariable_Method, ONLY: QuadratureVariable
USE MatmulUtility, ONLY: MyMatmul => Matmul
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_1
lg = MyMatmul(val, obj%dNdXi)
!! matmul r1 r3
END PROCEDURE elemsd_GetLocalGradient_1

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_2
lg = MyMatmul(val, obj%dNdXi)
!! matmul r2 r3
END PROCEDURE elemsd_GetLocalGradient_2

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_3
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = MyMatmul(MATMUL(val, obj%T), obj%dNdXi)
  !! matmul r1 r3
CLASS DEFAULT
#ifdef DEBUG_VER
  ERROR STOP 'GetLocalGradient: space-time object required'
#endif
END SELECT
END PROCEDURE elemsd_GetLocalGradient_3

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_4
SELECT TYPE (obj)
TYPE IS (STElemshapeData_)
  lg = MyMatmul(MyMatmul(val, obj%T), obj%dNdXi)
  !! (r3.r1).r3 => r2.r3
CLASS DEFAULT
#ifdef DEBUG_VER
  ERROR STOP 'GetLocalGradient: space-time object required'
#endif
END SELECT
END PROCEDURE elemsd_GetLocalGradient_4

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_5
#ifdef DEBUG_VER
CALL obj_CheckLocalGradientInput(obj, val)
#endif
SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  CALL Reallocate(lg, obj%xidim, obj%nips)

CASE (TypeFEVariableOpt%space)
  CALL GetLocalGradient(obj=obj, lg=lg, &
                        val=FeVariableGet(val, TypeFEVariableScalar, &
                                          TypeFEVariableSpace))

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL GetLocalGradient(obj=obj, lg=lg, &
                          val=FEVariableGet(val, TypeFEVariableScalar, &
                                            TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetLocalGradient_5

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_6
INTEGER(I4B) :: s(1)

#ifdef DEBUG_VER
CALL obj_CheckLocalGradientInput(obj, val)
#endif
SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  s = FeVariableShape(val)
  CALL Reallocate(lg, s(1), obj%xidim, obj%nips)

CASE (TypeFEVariableOpt%space)
  CALL GetLocalGradient(obj=obj, lg=lg, &
                        val=FEVariableGet(val, TypeFEVariableVector, &
                                          TypeFEVariableSpace))

CASE (TypeFeVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL GetLocalGradient(obj=obj, lg=lg, &
                          val=FEVariableGet(val, TypeFEVariableVector, &
                                            TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetLocalGradient_6

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_7
lg = MyMatmul(val, obj%dNdXi)
!! r3.r4
END PROCEDURE elemsd_GetLocalGradient_7

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_8
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  lg = MyMatmul(MyMatmul(val, obj%T), obj%dNdXi)
  !! (r4.r1).r3
CLASS DEFAULT
#ifdef DEBUG_VER
  ERROR STOP 'GetLocalGradient: space-time object required'
#endif
END SELECT
END PROCEDURE elemsd_GetLocalGradient_8

!----------------------------------------------------------------------------
!                                                           GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_9
INTEGER(I4B) :: s(2)
#ifdef DEBUG_VER
CALL obj_CheckLocalGradientInput(obj, val)
#endif
SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  s = FeVariableShape(val)
  CALL Reallocate(lg, s(1), s(2), obj%xidim, obj%nips)

CASE (TypeFEVariableOpt%space)
  CALL GetLocalGradient(obj=obj, lg=lg, &
                        val=FEVariableGet(val, TypeFEVariableMatrix, &
                                          TypeFEVariableSpace))

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL GetLocalGradient(obj=obj, lg=lg, &
                          val=FEVariableGet(val, TypeFEVariableMatrix, &
                                            TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE elemsd_GetLocalGradient_9

!----------------------------------------------------------------------------
!                                                         GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_10
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :)
!!
SELECT CASE (val%RANK)
CASE (TypeFEVariableOpt%scalar)
  CALL GetLocalGradient(obj=obj, lg=r2, val=val)
  lg = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)

CASE (TypeFeVariableOpt%vector)
  CALL GetLocalGradient(obj=obj, lg=r3, val=val)
  lg = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
  DEALLOCATE (r3)

CASE (TypeFeVariableOpt%matrix)
#ifdef DEBUG_VER
  ERROR STOP 'GetLocalGradient: matrix FEVariable result unsupported'
#endif
   !! TODO Extend FEVariable to support r3, which is necessary to keep
   !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_GetLocalGradient_10

!----------------------------------------------------------------------------
!                                                         GetLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetLocalGradient_11
REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
INTEGER(I4B) :: ii

SELECT CASE (val%RANK)

!! scalar
CASE (TypeFEVariableOpt%scalar)
  DO ii = 1, SIZE(obj)
    CALL GetLocalGradient(obj=obj(ii), lg=r2, val=val)
    IF (.NOT. ALLOCATED(r3)) THEN
      CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), SIZE(obj))
    END IF

    r3(:, :, ii) = r2(:, :)
  END DO
  lg = QuadratureVariable(r3, typeFEVariableVector, &
                          typeFEVariableSpaceTime)
  DEALLOCATE (r2, r3)

!! vector
CASE (TypeFEVariableOpt%vector)
  DO ii = 1, SIZE(obj)
    CALL GetLocalGradient(obj=obj(ii), lg=r3, val=val)
    IF (.NOT. ALLOCATED(r4)) THEN
      CALL Reallocate(r4, SIZE(r3, 1), SIZE(r3, 2), SIZE(r3, 3), SIZE(obj))
    END IF

    r4(:, :, :, ii) = r3(:, :, :)
  END DO
  lg = QuadratureVariable(r4, typeFEVariableMatrix, &
                          typeFEVariableSpaceTime)
  DEALLOCATE (r3, r4)

!! matrix TODO
CASE (TypeFEVariableOpt%matrix)
#ifdef DEBUG_VER
  ERROR STOP 'GetLocalGradient: matrix FEVariable result unsupported'
#endif
   !! TODO Extend FEVariable to support r3, which is necessary to keep
   !! the gradient of rank02 tensors
END SELECT
END PROCEDURE elemsd_GetLocalGradient_11

!----------------------------------------------------------------------------
!                                                              LocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalGradient_1
CALL GetLocalGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalGradient_1

!----------------------------------------------------------------------------
!                                                              LocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_LocalGradient_2
CALL GetLocalGradient(obj=obj, lg=ans, val=val)
END PROCEDURE elemsd_LocalGradient_2

!----------------------------------------------------------------------------
!                                               obj_CheckLocalGradientInput
!----------------------------------------------------------------------------

#ifdef DEBUG_VER
PURE SUBROUTINE obj_CheckLocalGradientInput(obj, val)
  CLASS(ElemShapeData_), INTENT(IN) :: obj
  !! element shape data
  TYPE(FEVariable_), INTENT(IN) :: val
  !! nodal constant, spatial, or space-time FEVariable

  IF (val%defineOn .NE. TypeFEVariableOpt%nodal) THEN
    ERROR STOP 'GetLocalGradient: unsupported FEVariable; use nodal values'
  END IF

  SELECT CASE (val%varType)
  CASE (TypeFEVariableOpt%constant, TypeFEVariableOpt%space)
    CONTINUE
  CASE (TypeFeVariableOpt%spacetime)
    SELECT TYPE (obj)
    TYPE IS (STElemShapeData_)
      CONTINUE
    CLASS DEFAULT
      ERROR STOP 'GetLocalGradient: unsupported FEVariable/object pairing'
    END SELECT
  CASE DEFAULT
    ERROR STOP 'GetLocalGradient: unsupported FEVariable type'
  END SELECT
END SUBROUTINE obj_CheckLocalGradientInput
#endif

END SUBMODULE Methods
