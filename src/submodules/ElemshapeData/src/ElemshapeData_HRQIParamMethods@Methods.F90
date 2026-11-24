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
INTEGER(I4B) :: ii, nips, nsd
REAL(DFP), ALLOCATABLE :: r0(:, :)
!! unitNormal (nsd, nips)
REAL(DFP), ALLOCATABLE :: G(:, :, :)
!! shape is (nsd, nsd, nips), it contains inverse of FFT
REAL(DFP), ALLOCATABLE :: FFT(:, :)
!! (nsd, nsd)
REAL(DFP), ALLOCATABLE :: rr(:, :)
!! outer product of unitNormal (nsd, nsd)
REAL(DFP) :: areal
LOGICAL(LGT) :: ismin, ismax
!!
!! Main
!!
nips = SIZE(obj%N, 2)
nsd = obj%nsd
CALL Reallocate(h, nips)
CALL Reallocate(G, nsd, nsd, nips)
CALL Reallocate(FFT, nsd, nsd)
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
IF (PRESENT(r)) r = r0
!!
!! FFT and G
!!
DO ii = 1, nips
  !!
  FFT = MATMUL(obj%jacobian(:, :, ii), &
    & TRANSPOSE(obj%jacobian(:, :, ii)))
  !!
  CALL Inv(invA=G(:, :, ii), A=FFT)
  !!
  rr = OUTERPROD(a=r0(1:nsd, ii), b=r0(1:nsd, ii))
  !!
  areal = Contraction(a1=G(:, :, ii), a2=rr)
  !!
  IF (areal.APPROXEQ.zero) THEN
    h(ii) = 0.0_DFP
  ELSE
    h(ii) = 2.0_DFP / SQRT(areal)
  END IF
  !!
END DO
!!
!! reset FFT to reuse it
!!
FFT = 0.0_DFP; r0 = 0.0_DFP
!!
IF (ismin .OR. ismax) THEN
  DO ii = 1, nips
    r0(:, ii) = SymEigenValuesUpto3(G(:, :, ii))
  END DO
END IF
!!
IF (ismax) THEN
  DO ii = 1, nips
    hmax(ii) = 2.0_DFP / SQRT(MINVAL(r0(:, ii)))
  END DO
END IF
!!
IF (ismin) THEN
  DO ii = 1, nips
    hmin(ii) = 2.0_DFP / SQRT(MAXVAL(r0(:, ii)))
  END DO
END IF
!!
IF (ALLOCATED(r0)) DEALLOCATE (r0)
IF (ALLOCATED(G)) DEALLOCATE (G)
IF (ALLOCATED(FFT)) DEALLOCATE (FFT)
IF (ALLOCATED(rr)) DEALLOCATE (rr)
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
