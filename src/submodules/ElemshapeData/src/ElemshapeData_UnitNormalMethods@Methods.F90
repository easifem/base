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

SUBMODULE(ElemshapeData_UnitNormalMethods) Methods
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE ElemshapeData_GradientMethods, ONLY: GetSpatialGradient
USE ElemshapeData_ScalarInterpolMethods, ONLY: GetInterpolation
USE ElemshapeData_VectorInterpolMethods, ONLY: GetInterpolation
! USE GlobalData, ONLY: Scalar
! USE GlobalData, ONLY: Vector
! USE GlobalData, ONLY: Zero
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            getUnitNormal_1
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_1
REAL(DFP), ALLOCATABLE :: dp(:, :), p(:)

CALL GetInterpolation(obj=obj, val=val, ans=p)
CALL GetSpatialGradient(obj=obj, ans=dp, val=val)
CALL Reallocate(R, obj%nsd, obj%nips)
CALL obj_ScalarUnitNormal(p, dp, R)
END PROCEDURE getUnitNormal_1

!----------------------------------------------------------------------------
!                                                            getUnitNormal_2
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_2
REAL(DFP), ALLOCATABLE :: dp(:, :, :), p(:, :)

CALL GetInterpolation(obj=obj, ans=p, val=val)
CALL GetSpatialGradient(obj=obj, ans=dp, val=val)
CALL Reallocate(R, obj%nsd, obj%nips)
CALL obj_VectorUnitNormal(p, dp, R)
END PROCEDURE getUnitNormal_2

!----------------------------------------------------------------------------
!                                                            getUnitNormal_3
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_3
IF (val%RANK .EQ. TypeFEVariableOpt%Scalar) THEN
  CALL obj_ScalarFEUnitNormal(obj, R, val)
ELSE IF (val%RANK .EQ. TypeFEVariableOpt%Vector) THEN
  CALL obj_VectorFEUnitNormal(obj, R, val)
END IF
END PROCEDURE getUnitNormal_3

!----------------------------------------------------------------------------
!                                                     obj_ScalarFEUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_ScalarFEUnitNormal(obj, R, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  !! element shape data
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
  !! unit normal at quadrature points
  TYPE(FEVariable_), INTENT(IN) :: val
  !! scalar nodal FEVariable

  REAL(DFP), ALLOCATABLE :: dp(:, :), p(:)

  CALL GetInterpolation(obj=obj, val=val, ans=p)
  CALL GetSpatialGradient(obj=obj, ans=dp, val=val, &
                          valRank=TypeFEVariableScalar)
  CALL Reallocate(R, obj%nsd, obj%nips)
  CALL obj_ScalarUnitNormal(p, dp, R)
END SUBROUTINE obj_ScalarFEUnitNormal

!----------------------------------------------------------------------------
!                                                     obj_VectorFEUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_VectorFEUnitNormal(obj, R, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  !! element shape data
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
  !! unit normal at quadrature points
  TYPE(FEVariable_), INTENT(IN) :: val
  !! vector nodal FEVariable

  REAL(DFP), ALLOCATABLE :: dp(:, :, :), p(:, :)

  CALL GetInterpolation(obj=obj, ans=p, val=val)
  CALL GetSpatialGradient(obj=obj, ans=dp, val=val, &
                          valRank=TypeFEVariableVector)
  CALL Reallocate(R, obj%nsd, obj%nips)
  CALL obj_VectorUnitNormal(p, dp, R)
END SUBROUTINE obj_VectorFEUnitNormal

!----------------------------------------------------------------------------
!                                                       obj_ScalarUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_ScalarUnitNormal(p, dp, R)
  REAL(DFP), INTENT(IN) :: p(:)
  !! scalar values at quadrature points
  REAL(DFP), INTENT(IN) :: dp(:, :)
  !! spatial gradients, dp(nsd, nips)
  REAL(DFP), INTENT(INOUT) :: R(:, :)
  !! existing output buffer, R(nsd, nips)

  ! Internal variables
  REAL(DFP), ALLOCATABLE :: pnorm(:)
  INTEGER(I4B) :: ii

  pnorm = NORM2(dp, DIM=1)
  DO ii = 1, SIZE(p)
    IF (pnorm(ii) .GT. math%zero) THEN
      IF (p(ii) .GE. 0.0_DFP) THEN
        R(:, ii) = dp(:, ii) / pnorm(ii)
      ELSE
        R(:, ii) = -dp(:, ii) / pnorm(ii)
      END IF
    END IF
  END DO
END SUBROUTINE obj_ScalarUnitNormal

!----------------------------------------------------------------------------
!                                                       obj_VectorUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_VectorUnitNormal(p, dp, R)
  REAL(DFP), INTENT(INOUT) :: p(:, :)
!! vector values, normalized in place, p(ncomp, nips)
  REAL(DFP), INTENT(IN) :: dp(:, :, :)
!! spatial gradients, dp(ncomp, nsd, nips)
  REAL(DFP), INTENT(INOUT) :: R(:, :)
!! existing output buffer, R(nsd, nips)

  ! Internal variables
  REAL(DFP), ALLOCATABLE :: pnorm(:)
  REAL(DFP) :: mv(SIZE(dp, 2)), nrm
  INTEGER(I4B) :: i

  pnorm = NORM2(p, DIM=1)
  quadraturePoints: DO i = 1, SIZE(pnorm)
    IF (pnorm(i) .GT. math%zero) THEN
      p(:, i) = p(:, i) / pnorm(i)
    ELSE
      R(:, i) = math%zero
      CYCLE quadraturePoints
    END IF
    mv = MATMUL(p(:, i), dp(:, :, i))
    nrm = NORM2(mv)
    IF (nrm .GT. math%zero) THEN
      R(:, i) = mv / nrm
    END IF
  END DO quadraturePoints
END SUBROUTINE obj_VectorUnitNormal

END SUBMODULE Methods
