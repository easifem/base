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

SUBMODULE(ElemshapeData_HRQIParamMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getHRQIParam1
INTEGER(I4B) :: ii, nips, nsd, xidim
REAL(DFP), ALLOCATABLE :: r0(:, :)
!! unitNormal (nsd, nips)
REAL(DFP), ALLOCATABLE :: C(:, :)
!! TRANSPOSE(J) * J, shape (xidim, xidim)
REAL(DFP), ALLOCATABLE :: invC(:, :)
!! inverse of C, shape (xidim, xidim)
REAL(DFP), ALLOCATABLE :: v(:)
!! invC * TRANSPOSE(J) * r, shape (xidim)
REAL(DFP), ALLOCATABLE :: w(:)
!! eigenvalues of C, shape (xidim)
REAL(DFP) :: areal
LOGICAL(LGT) :: ismin, ismax
!!
!! Main
!!
nips = SIZE(obj%N, 2)
nsd = obj%nsd
xidim = SIZE(obj%jacobian, 2)
CALL Reallocate(h, nips)
CALL Reallocate(C, xidim, xidim)
CALL Reallocate(invC, xidim, xidim)
!!
!! hmax
!!
IF (PRESENT(hmax)) THEN
  CALL Reallocate(hmax, nips)
  ismax = .TRUE.
ELSE
  ismax = .FALSE.
END IF
!!
!! hmin
!!
IF (PRESENT(hmin)) THEN
  CALL Reallocate(hmin, nips)
  ismin = .TRUE.
ELSE
  ismin = .FALSE.
END IF
!!
!! r and unitNormal
!!
CALL GetUnitNormal(obj=obj, val=val, r=r0)
!!
!! GetUnitNormal leaves r0 unallocated for a val that is neither a
!! scalar nor a vector. Then there is no direction: r0 = 0, so h = 0.
!!
IF (.NOT. ALLOCATED(r0)) THEN
  CALL Reallocate(r0, nsd, nips)
  r0 = 0.0_DFP
END IF
IF (PRESENT(r)) r = r0
!!
!! C = TRANSPOSE(J) * J is xidim x xidim and regular when the map is
!! regular, also for xidim < nsd. The metric G = J * invC**2 *
!! TRANSPOSE(J) is the pseudo-inverse of J * TRANSPOSE(J), so
!! r^T G r = |invC * TRANSPOSE(J) * r|**2.
!!
DO ii = 1, nips
  !!
  C = MATMUL(TRANSPOSE(obj%jacobian(:, :, ii)), obj%jacobian(:, :, ii))
  !!
  CALL Inv(invA=invC, A=C)
  !!
  v = MATMUL(invC, MATMUL(r0(1:nsd, ii), obj%jacobian(:, :, ii)))
  !!
  areal = DOT_PRODUCT(v, v)
  !!
  !! zero direction: areal is zero up to round-off
  IF (areal .LE. zero) THEN
    h(ii) = 0.0_DFP
  ELSE
    h(ii) = 2.0_DFP / SQRT(areal)
  END IF
  !!
  !! eigenvalues of C are the reciprocals of the nonzero eigenvalues of G
  !!
  IF (ismin .OR. ismax) THEN
    w = SymEigenValuesUpto3(C)
    IF (ismax) hmax(ii) = 2.0_DFP * SQRT(MAXVAL(w))
    IF (ismin) hmin(ii) = 2.0_DFP * SQRT(MINVAL(w))
  END IF
  !!
END DO
!!
IF (ALLOCATED(r0)) DEALLOCATE (r0)
IF (ALLOCATED(C)) DEALLOCATE (C)
IF (ALLOCATED(invC)) DEALLOCATE (invC)
IF (ALLOCATED(v)) DEALLOCATE (v)
IF (ALLOCATED(w)) DEALLOCATE (w)
END PROCEDURE elemsd_getHRQIParam1

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam2
REAL(DFP), ALLOCATABLE :: h0(:), hmax0(:), hmin0(:), r0(:, :)

CALL GetHRQIParam(obj=obj, h=h0, hmax=hmax0, hmin=hmin0, r=r0, val=val, &
                  opt=opt)

h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)

IF (PRESENT(hmax)) hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
                                             TypeFEVariableSpace)

IF (PRESENT(hmin)) hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
                                             TypeFEVariableSpace)

IF (PRESENT(r)) r = QuadratureVariable(r0, TypeFEVariableVector, &
                                       TypeFEVariableSpace)
END PROCEDURE elemsd_GetHRQIParam2

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam3
INTEGER(I4B) :: ii, nips, nipt, nsd
REAL(DFP), ALLOCATABLE :: h0(:), hmax0(:), hmin0(:), r0(:, :)

nips = SIZE(obj(1)%N, 2)
nipt = SIZE(obj)
nsd = obj(1)%nsd

CALL Reallocate(h, nips, nipt)
IF (PRESENT(hmax)) CALL Reallocate(hmax, nips, nipt)
IF (PRESENT(hmin)) CALL Reallocate(hmin, nips, nipt)
IF (PRESENT(r)) CALL Reallocate(r, nsd, nips, nipt)

DO ii = 1, nipt
  CALL GetHRQIParam(obj=obj(ii), h=h0, hmax=hmax0, hmin=hmin0, r=r0, &
                    val=val, opt=opt)
  h(:, ii) = h0(:)
  IF (PRESENT(hmax)) hmax(:, ii) = hmax0(:)
  IF (PRESENT(hmin)) hmin(:, ii) = hmin0(:)
  IF (PRESENT(r)) r(:, :, ii) = r0(:, :)
END DO
END PROCEDURE elemsd_GetHRQIParam3

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam4
REAL(DFP), ALLOCATABLE :: h0(:, :), hmax0(:, :), hmin0(:, :), r0(:, :, :)

CALL GetHRQIParam(obj=obj, h=h0, hmax=hmax0, hmin=hmin0, r=r0, val=val, &
                  opt=opt)

h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpaceTime)

IF (PRESENT(hmax)) hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
                                             TypeFEVariableSpaceTime)

IF (PRESENT(hmin)) hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
                                             TypeFEVariableSpaceTime)

IF (PRESENT(r)) r = QuadratureVariable(r0, TypeFEVariableVector, &
                                       TypeFEVariableSpaceTime)
END PROCEDURE elemsd_GetHRQIParam4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
