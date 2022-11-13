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

SUBMODULE(ElemshapeData_Method) HRQIParamMethods
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
nsd = obj%refelem%nsd
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
    CALL JacobiMethod( &
      & mat=G(:, :, ii), &
      & eigenValues=r0(:, ii), &
      & eigenVectors=FFT, &
      & maxIter=100)
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
DEALLOCATE (r0, G, FFT, rr)
END PROCEDURE elemsd_getHRQIParam1

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam2
  !!
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: h0(:), hmax0(:), hmin0(:), r0(:, :)
CHARACTER(LEN=3) :: cod
  !!
  !! main
  !!
cod = "FFF"
  !!
IF (PRESENT(hmax)) THEN
  cod(1:1) = "T"
END IF
  !!
IF (PRESENT(hmin)) THEN
  cod(2:2) = "T"
END IF
  !!
IF (PRESENT(r)) THEN
  cod(3:3) = "T"
END IF
  !!
SELECT CASE (cod)
CASE ("FFF")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
    !!
CASE ("TFF")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
      !!
CASE ("FTF")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmin=hmin0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
      !!
CASE ("TTF")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & hmin=hmin0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
      !!
CASE ("FFT")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  r = QuadratureVariable(r0, TypeFEVariableVector, TypeFEVariableSpace)
      !!
CASE ("TFT")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
  r = QuadratureVariable(r0, TypeFEVariableVector, TypeFEVariableSpace)
      !!
CASE ("FTT")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmin=hmin0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
  r = QuadratureVariable(r0, TypeFEVariableVector, TypeFEVariableSpace)
      !!
CASE ("TTT")
      !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & hmin=hmin0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
      !!
  h = QuadratureVariable(h0, TypeFEVariableScalar, TypeFEVariableSpace)
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
  r = QuadratureVariable(r0, TypeFEVariableVector, TypeFEVariableSpace)
      !!
END SELECT
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
IF (ALLOCATED(r0)) DEALLOCATE (r0)
  !!
END PROCEDURE elemsd_GetHRQIParam2

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam3
  !!
INTEGER(I4B) :: ii, nips, nipt, nsd
REAL(DFP), ALLOCATABLE :: h0(:), hmax0(:), hmin0(:), r0(:, :)
CHARACTER(LEN=3) :: cod
  !!
  !! main
  !!
nips = SIZE(obj(1)%N, 2)
nipt = SIZE(obj)
nsd = obj(1)%refelem%nsd
  !!
CALL Reallocate(h, nips, nipt)
  !!
cod = "FFF"
  !!
IF (PRESENT(hmax)) THEN
  CALL Reallocate(hmax, nips, nipt)
  cod(1:1) = "T"
END IF
  !!
IF (PRESENT(hmin)) THEN
  CALL Reallocate(hmin, nips, nipt)
  cod(2:2) = "T"
END IF
  !!
IF (PRESENT(r)) THEN
  CALL Reallocate(r, nsd, nips, nipt)
  cod(3:3) = "T"
END IF
  !!
SELECT CASE (cod)
CASE ("FFF")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
      !!
  END DO
    !!
CASE ("TFF")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmax=hmax0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmax(:, ii) = hmax0(:)
      !!
  END DO
    !!
CASE ("FTF")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmin=hmin0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmin(:, ii) = hmin0(:)
      !!
  END DO
    !!
CASE ("TTF")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmax=hmax0, &
      & hmin=hmin0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmax(:, ii) = hmax0(:)
    hmin(:, ii) = hmin0(:)
      !!
  END DO
    !!
CASE ("FFT")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & r=r0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    r(:, :, ii) = r0(:, :)
      !!
  END DO
    !!
CASE ("TFT")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmax=hmax0, &
      & r=r0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmax(:, ii) = hmax0(:)
    r(:, :, ii) = r0(:, :)
      !!
  END DO
    !!
CASE ("FTT")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmin=hmin0, &
      & r=r0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmin(:, ii) = hmin0(:)
    r(:, :, ii) = r0(:, :)
      !!
  END DO
    !!
CASE ("TTT")
    !!
  DO ii = 1, SIZE(obj)
      !!
    CALL GetHRQIParam( &
      & obj=obj(ii), &
      & h=h0, &
      & hmax=hmax0, &
      & hmin=hmin0, &
      & r=r0, &
      & val=val, &
      & opt=opt)
      !!
    h(:, ii) = h0(:)
    hmax(:, ii) = hmax0(:)
    hmin(:, ii) = hmin0(:)
    r(:, :, ii) = r0(:, :)
      !!
  END DO
    !!
END SELECT
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
IF (ALLOCATED(r0)) DEALLOCATE (r0)
  !!
END PROCEDURE elemsd_GetHRQIParam3

!----------------------------------------------------------------------------
!                                                               getHRQIParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRQIParam4
  !!
REAL(DFP), ALLOCATABLE :: h0(:, :), hmax0(:, :), hmin0(:, :), &
  & r0(:, :, :)
CHARACTER(LEN=3) :: cod
  !!
  !! main
  !!
cod = "FFF"
  !!
IF (PRESENT(hmax)) THEN
  cod(1:1) = "T"
END IF
  !!
IF (PRESENT(hmin)) THEN
  cod(2:2) = "T"
END IF
  !!
IF (PRESENT(r)) THEN
  cod(3:3) = "T"
END IF
  !!
SELECT CASE (cod)
CASE ("FFF")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
    !!
CASE ("TFF")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmax = QuadratureVariable( &
    & hmax0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("FTF")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmin=hmin0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmin = QuadratureVariable( &
    & hmin0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("TTF")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & hmin=hmin0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmax = QuadratureVariable( &
    & hmax0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmin = QuadratureVariable( &
    & hmin0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("FFT")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  r = QuadratureVariable( &
    & r0, &
    & TypeFEVariableVector, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("TFT")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmax = QuadratureVariable( &
    & hmax0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  r = QuadratureVariable( &
    & r0, &
    & TypeFEVariableVector, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("FTT")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmin=hmin0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmin = QuadratureVariable( &
    & hmin0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  r = QuadratureVariable( &
    & r0, &
    & TypeFEVariableVector, &
    & TypeFEVariableSpaceTime)
    !!
CASE ("TTT")
    !!
  CALL GetHRQIParam( &
    & obj=obj, &
    & h=h0, &
    & hmax=hmax0, &
    & hmin=hmin0, &
    & r=r0, &
    & val=val, &
    & opt=opt)
    !!
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  hmin = QuadratureVariable( &
    & hmin0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
    !!
  hmax = QuadratureVariable( &
    & hmax0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
    !!
  r = QuadratureVariable( &
    & r0, &
    & TypeFEVariableVector, &
    & TypeFEVariableSpaceTime)
    !!
END SELECT
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
IF (ALLOCATED(r0)) DEALLOCATE (r0)
  !!
END PROCEDURE elemsd_GetHRQIParam4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HRQIParamMethods
