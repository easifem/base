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

SUBMODULE(ElemshapeData_Method) StabilizationParamMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_1
! Define internal variables
REAL(DFP), ALLOCATABLE :: dp(:, :), p(:), pnorm(:)
INTEGER(I4B) :: ii
!! main
CALL getInterpolation(obj=obj, Val=val, Interpol=p)
CALL getSpatialGradient(obj=obj, lg=dp, Val=Val)
CALL Reallocate(R, obj%refelem%NSD, SIZE(obj%N, 2))
pnorm = NORM2(dp, DIM=1)
!!
DO ii = 1, SIZE(p)
  IF (pnorm(ii) .GT. zero) THEN
    IF (p(ii) .GE. 0.0_DFP) THEN
      R(:, ii) = dp(:, ii) / pnorm(ii)
    ELSE
      R(:, ii) = -dp(:, ii) / pnorm(ii)
    END IF
  END IF
END DO
!!
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END PROCEDURE getUnitNormal_1

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_2
REAL(DFP), ALLOCATABLE :: dp(:, :, :)
REAL(DFP), ALLOCATABLE :: p(:, :)
REAL(DFP), ALLOCATABLE :: mv(:)
REAL(DFP), ALLOCATABLE :: pnorm(:)
REAL(DFP) :: nrm
INTEGER(I4B) :: i
!! main
!! interpolate the vector
CALL getInterpolation(obj=obj, Interpol=p, Val=val)
!! get gradient of nodal values
CALL getSpatialGradient(obj=obj, lg=dp, Val=val)
pnorm = NORM2(p, DIM=1)
CALL Reallocate(R, obj%RefElem%NSD, SIZE(obj%N, 2))
DO i = 1, SIZE(pnorm)
  IF (pnorm(i) .GT. Zero) THEN
    p(:, i) = p(:, i) / pnorm(i)
  ELSE
    p(:, i) = 1.0
  END IF
  mv = MATMUL(p(:, i), dp(:, :, i))
  nrm = NORM2(mv)
  IF (nrm .GT. Zero) THEN
    R(:, i) = mv / nrm
  END IF
END DO
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(mv)) DEALLOCATE (mv)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END PROCEDURE getUnitNormal_2

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_3
  !!
IF (val%rank .EQ. scalar) THEN
  CALL scalar_getUnitNormal_3(obj=obj, r=r, val=val)
ELSEIF (val%rank .EQ. vector) THEN
  CALL vector_getUnitNormal_3(obj=obj, r=r, val=val)
END IF
  !!
CONTAINS
  !!
PURE SUBROUTINE scalar_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_1.inc"
END SUBROUTINE scalar_getUnitNormal_3
  !!
PURE SUBROUTINE vector_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_2.inc"
END SUBROUTINE vector_getUnitNormal_3
  !!
END PROCEDURE getUnitNormal_3

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getHRGNParam_a(obj, h, val, opt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
  TYPE(FEVariable_), INTENT(IN) :: val
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! define internal variables
  !!
  INTEGER(I4B) :: ii
  REAL(DFP) :: areal
  REAL(DFP), ALLOCATABLE :: q(:, :)
  !! rdNdXt; (I,ips) => projection of dNdXt on unit normal
  TYPE(FEVariable_) :: rvar
  !! vector variable for keeping r
  !!
  !! Main
  !!
  CALL Reallocate(h, SIZE(obj%N, 2))
  !!
  !! Get unitNormal in q
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=q)
  !!
  !! Convert unit normal to [[FEVariable_]]
  !!
  rvar = QuadratureVariable(q, TypeFEVariableVector, TypeFEVariableSpace)
  !!
  !! Call get projection of dNdXt in q
  !!
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
  !!
  DO ii = 1, SIZE(h)
    areal = SUM(ABS(q(:, ii)))
    IF (areal.APPROXEQ.zero) THEN
      h(ii) = 0.0_DFP
    ELSE
      h(ii) = 2.0_DFP / areal
    END IF
  END DO
  !!
  IF (ALLOCATED(q)) DEALLOCATE (q)
  CALL DEALLOCATE (rvar)
  !!
END SUBROUTINE elemsd_getHRGNParam_a

!----------------------------------------------------------------------------
!                                                              getHRGNParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getHRGNParam_b(obj, h, val, opt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
  TYPE(FEVariable_), INTENT(IN) :: val
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! internal variables
  !!
  INTEGER(I4B) :: ii
  REAL(DFP) :: areal
  REAL(DFP), ALLOCATABLE :: r(:, :)
  REAL(DFP), ALLOCATABLE :: q(:, :, :)
  !! rdNTdXt; (I,a,ips)m => projection of dNTdXt on unit normal
  TYPE(FEVariable_) :: rvar
  !!
  !! main
  !!
  CALL Reallocate(h, SIZE(obj%N, 2))
  !!
  !! Get unitNormal in r
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  !!
  !! Make [[FEVariable_]]
  !!
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  !!
  !! Get Projection of dNTdXt in q
  !!
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=q, val=rvar)
  !!
  DO ii = 1, SIZE(h, 1)
    areal = SUM(ABS(q(:, :, ii)))
    IF (areal.APPROXEQ.zero) THEN
      h(ii) = 0.0_DFP
    ELSE
      h(ii) = 2.0_DFP / areal
    END IF
  END DO
  !!
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  CALL DEALLOCATE (rvar)
  !!
END SUBROUTINE elemsd_getHRGNParam_b

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getHRGNParam1
SELECT TYPE (obj)
!!
TYPE IS (ElemshapeData_)
  !!
  CALL elemsd_getHRGNParam_a( &
    & obj=obj, &
    & h=h, &
    & val=val, &
    & opt=opt)
  !!
CLASS IS (STElemshapeData_)
  !!
  CALL elemsd_getHRGNParam_b( &
    & obj=obj, &
    & h=h, &
    & val=val, &
    & opt=opt)
  !!
END SELECT
END PROCEDURE elemsd_getHRGNParam1

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam2
REAL(DFP), ALLOCATABLE :: ans(:)
!!
CALL GetHRGNParam(obj=obj, h=ans, val=val, opt=opt)
h = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
IF (ALLOCATED(ans)) DEALLOCATE (ans)
!!
END PROCEDURE elemsd_GetHRGNParam2

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam3
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: avec(:)
!!
!! main
!!
CALL Reallocate(h, SIZE(obj(1)%N, 2), SIZE(obj))
!!
DO ii = 1, SIZE(obj)
  CALL GetHRGNParam( &
    & obj=obj(ii), &
    & h=avec, &
    & val=val, &
    & opt=opt)
  !!
  h(:, ii) = avec(:)
END DO
!!
IF (ALLOCATED(avec)) DEALLOCATE (avec)
END PROCEDURE elemsd_GetHRGNParam3

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam4
REAL(DFP), ALLOCATABLE :: ans(:, :)
!!
CALL GetHRGNParam(obj=obj, h=ans, val=val, opt=opt)
!!
h = QuadratureVariable( &
  & ans, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
IF (ALLOCATED(ans)) DEALLOCATE (ans)
END PROCEDURE elemsd_GetHRGNParam4

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
!                                                              getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_1
REAL(DFP), ALLOCATABLE :: h0(:), nubar(:)
INTEGER(I4B) :: ii
!!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
!!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
!!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
!!
DO ii = 1, SIZE(h0)
  h0(ii) = h0(ii)**2 / nubar(ii)
END DO
!!
tau = QuadratureVariable(h0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
!!
DEALLOCATE (h0, nubar)
END PROCEDURE elemsd_GetSUGN3Param_1

!----------------------------------------------------------------------------
!                                                             getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_2
  !!
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: h0(:, :), nubar(:, :)
  !!
  !! main
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
DO ii = 1, SIZE(obj)
  h0(:, ii) = h0(:, ii)**2 / nubar(:, ii)
END DO
  !!
tau = QuadratureVariable( &
  & h0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  !!
END PROCEDURE elemsd_GetSUGN3Param_2

!----------------------------------------------------------------------------
!                                                              getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_3
  !!
REAL(DFP), ALLOCATABLE :: h0(:)
INTEGER(I4B) :: ii
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
DO ii = 1, SIZE(h0)
  h0(ii) = h0(ii)**2 / nu
END DO
  !!
tau = QuadratureVariable(h0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
  !!
DEALLOCATE (h0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_3

!----------------------------------------------------------------------------
!                                                             getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_4
  !!
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: h0(:, :)
  !!
  !! main
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
DO ii = 1, SIZE(obj)
  h0(:, ii) = h0(:, ii)**2 / nu
END DO
  !!
tau = QuadratureVariable( &
  & h0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_4

!----------------------------------------------------------------------------
!                                                getSUGN3Param_Takizawa2018
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_1
  !!
REAL(DFP), ALLOCATABLE :: nubar(:), h0(:), hmax0(:), hmin0(:), &
  & r0(:, :), tau0(:)
INTEGER(I4B) :: ii, nips
REAL(DFP) :: areal, r2
  !!
CALL GetHRQIParam( &
  & obj=obj, &
  & h=h0, &
  & val=val, &
  & hmax=hmax0, &
  & hmin=hmin0, &
  & r=r0, &
  & opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
IF (PRESENT(hmax)) THEN
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
IF (PRESENT(hmin)) THEN
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
CALL Reallocate(tau0, SIZE(h0))
  !!
DO ii = 1, SIZE(h0)
    !!
  r2 = DOT_PRODUCT(r0(:, ii), r0(:, ii))
    !!
  IF (h0(ii) .APPROXEQ.zero) THEN
    tau0(ii) = 4.0_DFP * nubar(ii) * &
      & (1.0_DFP - r2) / hmin0(ii)**2
  ELSE
    tau0(ii) = 4.0_DFP * nubar(ii) * &
      & ((1.0_DFP - r2) / hmin0(ii)**2 &
      & + 1.0_DFP / h0(ii)**2)
  END IF
    !!
  tau0(ii) = 1.0_DFP / tau0(ii)
    !!
END DO
  !!
tau = QuadratureVariable(tau0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
  !!
DEALLOCATE (nubar, h0, hmax0, hmin0, r0, tau0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_1

!----------------------------------------------------------------------------
!                                                getSUGN3Param_Takizawa2018
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_2
  !!
REAL(DFP), ALLOCATABLE :: nubar(:, :), h0(:, :), hmax0(:, :), &
  & hmin0(:, :), r0(:, :, :), tau0(:, :)
INTEGER(I4B) :: ii, nipt, nips, ipt
REAL(DFP) :: areal, r2
  !!
nipt = SIZE(obj)
  !!
CALL GetHRQIParam( &
  & obj=obj, &
  & h=h0, &
  & val=val, &
  & hmax=hmax0, &
  & hmin=hmin0, &
  & r=r0, &
  & opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
IF (PRESENT(hmax)) THEN
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
IF (PRESENT(hmin)) THEN
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
nips = SIZE(h0, 1)
  !!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
CALL Reallocate(tau0, nips, nipt)
  !!
DO ipt = 1, nipt
  DO ii = 1, nips
      !!
    r2 = DOT_PRODUCT(r0(:, ii, ipt), r0(:, ii, ipt))
      !!
    IF (h0(ii, ipt) .APPROXEQ.zero) THEN
      tau0(ii, ipt) = 4.0_DFP * nubar(ii, ipt) * &
        & (1.0_DFP - r2) / hmin0(ii, ipt)**2
    ELSE
      tau0(ii, ipt) = 4.0_DFP * nubar(ii, ipt) * &
        & ((1.0_DFP - r2) / hmin0(ii, ipt)**2 &
        & + 1.0_DFP / h0(ii, ipt)**2)
    END IF
      !!
    tau0(ii, ipt) = 1.0_DFP / tau0(ii, ipt)
      !!
  END DO
END DO
  !!
tau = QuadratureVariable(tau0, TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
DEALLOCATE (nubar, h0, hmax0, hmin0, r0, tau0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_2

!----------------------------------------------------------------------------
!                                                getSUGN3Param_Takizawa2018
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_3
  !!
REAL(DFP), ALLOCATABLE :: h0(:), hmax0(:), hmin0(:), &
  & r0(:, :), tau0(:)
INTEGER(I4B) :: ii
REAL(DFP) :: areal, r2
  !!
CALL GetHRQIParam( &
  & obj=obj, &
  & h=h0, &
  & val=val, &
  & hmax=hmax0, &
  & hmin=hmin0, &
  & r=r0, &
  & opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
IF (PRESENT(hmax)) THEN
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
IF (PRESENT(hmin)) THEN
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
CALL Reallocate(tau0, SIZE(h0))
  !!
DO ii = 1, SIZE(h0)
    !!
  r2 = DOT_PRODUCT(r0(:, ii), r0(:, ii))
    !!
  IF (h0(ii) .APPROXEQ.zero) THEN
    tau0(ii) = 4.0_DFP * nu * &
      & (1.0_DFP - r2) / hmin0(ii)**2
  ELSE
    tau0(ii) = 4.0_DFP * nu * &
      & ((1.0_DFP - r2) / hmin0(ii)**2 &
      & + 1.0_DFP / h0(ii)**2)
  END IF
    !!
  tau0(ii) = 1.0_DFP / tau0(ii)
    !!
END DO
  !!
tau = QuadratureVariable(tau0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
  !!
DEALLOCATE (h0, hmax0, hmin0, r0, tau0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_3

!----------------------------------------------------------------------------
!                                                getSUGN3Param_Takizawa2018
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_4
  !!
REAL(DFP), ALLOCATABLE :: h0(:, :), hmax0(:, :), &
  & hmin0(:, :), r0(:, :, :), tau0(:, :)
INTEGER(I4B) :: ii, nipt, nips, ipt
REAL(DFP) :: areal, r2
  !!
nipt = SIZE(obj)
  !!
CALL GetHRQIParam( &
  & obj=obj, &
  & h=h0, &
  & val=val, &
  & hmax=hmax0, &
  & hmin=hmin0, &
  & r=r0, &
  & opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
IF (PRESENT(hmax)) THEN
  hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
IF (PRESENT(hmin)) THEN
  hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
nips = SIZE(h0, 1)
  !!
CALL Reallocate(tau0, nips, nipt)
  !!
DO ipt = 1, nipt
  DO ii = 1, nips
      !!
    r2 = DOT_PRODUCT(r0(:, ii, ipt), r0(:, ii, ipt))
      !!
    IF (h0(ii, ipt) .APPROXEQ.zero) THEN
      tau0(ii, ipt) = 4.0_DFP * nu * &
        & (1.0_DFP - r2) / hmin0(ii, ipt)**2
    ELSE
      tau0(ii, ipt) = 4.0_DFP * nu * &
        & ((1.0_DFP - r2) / hmin0(ii, ipt)**2 &
        & + 1.0_DFP / h0(ii, ipt)**2)
    END IF
      !!
    tau0(ii, ipt) = 1.0_DFP / tau0(ii, ipt)
      !!
  END DO
END DO
  !!
tau = QuadratureVariable(tau0, TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
DEALLOCATE (h0, hmax0, hmin0, r0, tau0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_4

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_a(obj, tau, c, val, nu, k, &
  & phi, dt, opt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  !! element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilizing parameters
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  TYPE(FEVariable_), INTENT(IN) :: nu
  !! diffusivity coefficient
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
  !! permeability
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
  !! porosity
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time step size
  !! default value is zero
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! default=1
  !! if opt=1, then we use `SQRT(1.0_DFP / (t1**2 + t2**2 + t3**2 + t4**2))`
  !! if opt=2, then we use `1.0_DFP / (t1 + t2 + t3 + t4)`
  !!
  !!
  !! define internal variables
  !!
  INTEGER(I4B) :: ii, opt0
  REAL(DFP) :: t1, t2, t3, t4
  REAL(DFP), ALLOCATABLE :: p(:, :)
  !! cdNdXt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! unit normal
  REAL(DFP), ALLOCATABLE :: q(:, :)
  !! rdNdXt
  REAL(DFP), ALLOCATABLE :: ans(:)
  !! result
  REAL(DFP), ALLOCATABLE :: nubar(:), kbar(:), phibar(:)
  !! value of nu at space quadrature points
  TYPE(FEVariable_) :: rvar
  !! vector variable for keeping r
  !!
  !! Main
  !!
  opt0 = INPUT(default=1_I4B, option=opt)
  !!
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=p, val=c)
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
  !!
  CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
  IF (PRESENT(k)) THEN
    CALL GetInterpolation(obj=obj, val=k, interpol=kbar)
    CALL GetInterpolation(obj=obj, val=phi, interpol=phibar)
  ELSE
    ALLOCATE (kbar(SIZE(nubar)))
    ALLOCATE (phibar(SIZE(nubar)))
    kbar = MaxDFP !! very large number
    phibar = 1.0_DFP
  END IF
  !!
  t2 = 0.0_DFP
  IF (PRESENT(dt)) THEN
    IF (dt .GT. zero) t2 = 2.0_DFP / dt
  END IF
  !!
  CALL Reallocate(ans, SIZE(obj%N, 2))
  !!
  IF (opt0 .EQ. 1_I4B) THEN
    DO ii = 1, SIZE(ans)
      t1 = SUM(ABS(p(:, ii)))
      t3 = nubar(ii) * (SUM(ABS(q(:, ii))))**2
      t4 = 2.0_DFP * phibar(ii) * nubar(ii) / kbar(ii)
      ans(ii) = SQRT(1.0_DFP / (t1**2 + t2**2 + t3**2 + t4**2))
    END DO
  ELSE
    DO ii = 1, SIZE(ans)
      t1 = SUM(ABS(p(:, ii)))
      t3 = nubar(ii) * (SUM(ABS(q(:, ii))))**2
      t4 = 2.0_DFP * phibar(ii) * nubar(ii) / kbar(ii)
      ans(ii) = 1.0_DFP / (t1 + t2 + t3 + t4)
    END DO
  END IF
  !!
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
  !! cleanup
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  IF (ALLOCATED(kbar)) DEALLOCATE (kbar)
  IF (ALLOCATED(phibar)) DEALLOCATE (phibar)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_a

!----------------------------------------------------------------------------
!                                                              getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_b(obj, tau, c, val, nu, k, &
  & phi, dt, opt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  !! space-time element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilization parameter
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  !! scalar/vector variable
  TYPE(FEVariable_), INTENT(IN) :: nu
  !! diffusivity
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
  !! permeability
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
  !! porosity
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time-step size
  !! This parameter is not used currently.
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! default=1
  !! opt=1, then `ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))`
  !! opt=2, then `ans(ii) = 1.0_DFP / (t12 + t3 + t4)`
  !!
  !! INTERNAL VARIABLES
  !!
  INTEGER(I4B) :: ii, opt0
  REAL(DFP) :: t12, t3, t4
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  !! cdNTdxt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! vector at space quad points
  REAL(DFP), ALLOCATABLE :: q(:, :, :)
  !!
  REAL(DFP), ALLOCATABLE :: ans(:)
  REAL(DFP), ALLOCATABLE :: nubar(:)
  REAL(DFP), ALLOCATABLE :: kbar(:)
  REAL(DFP), ALLOCATABLE :: phibar(:)
  TYPE(FEVariable_) :: rvar
  !!
  !! MAIN
  !!
  opt0 = INPUT(option=opt, default=1_I4B)
  !!
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=p, val=c)
  !!
  !! make cdNTdxt + dNTdt
  !!
  p = p + obj%dNTdt
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=q, val=rvar)
  CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
  IF (PRESENT(k)) THEN
    CALL GetInterpolation(obj=obj, val=k, interpol=kbar)
    CALL GetInterpolation(obj=obj, val=phi, interpol=phibar)
  ELSE
    ALLOCATE (kbar(SIZE(nubar)))
    ALLOCATE (phibar(SIZE(nubar)))
    kbar = MaxDFP !! very large number
    phibar = 1.0_DFP
  END IF
  !!
  CALL reallocate(ans, SIZE(obj%N, 2))
  !!
  IF (opt0 .EQ. 1_I4B) THEN
    DO ii = 1, SIZE(ans, 1)
      t12 = SUM(ABS(p(:, :, ii)))
      t3 = nubar(ii) * (SUM(ABS(q(:, :, ii))))**2
      t4 = 2.0_DFP * phibar(ii) * nubar(ii) / kbar(ii)
      ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))
    END DO
  ELSE
    DO ii = 1, SIZE(ans, 1)
      t12 = SUM(ABS(p(:, :, ii)))
      t3 = nubar(ii) * (SUM(ABS(q(:, :, ii))))**2
      t4 = 2.0_DFP * phibar(ii) * nubar(ii) / kbar(ii)
      ans(ii) = 1.0_DFP / (t12 + t3 + t4)
    END DO
  END IF
  !!
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
  !! cleanup
  !!
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  IF (ALLOCATED(kbar)) DEALLOCATE (kbar)
  IF (ALLOCATED(phibar)) DEALLOCATE (phibar)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSUPGParam1
SELECT TYPE (obj)
TYPE IS (ElemshapeData_)
  !!
  CALL elemsd_getSUPGParam_a( &
    & obj=obj, &
    & tau=tau, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
  !!
CLASS IS (STElemshapeData_)
  !!
  CALL elemsd_getSUPGParam_b( &
    & obj=obj, &
    & tau=tau, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
  !!
END SELECT
END PROCEDURE elemsd_getSUPGParam1

!----------------------------------------------------------------------------
!                                                             GetSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUPGParam2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: ans(:, :)
TYPE(FEVariable_) :: a
!!
!! main
!!
CALL Reallocate(ans, SIZE(obj(1)%N, 2), SIZE(obj))
!!
DO ii = 1, SIZE(obj)
  !!
  CALL elemsd_getSUPGParam_b( &
    & obj=obj(ii), &
    & tau=a, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
  !!
  ans(:, ii) = Get(a, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
END DO
!!
tau = QuadratureVariable( &
  & ans, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
CALL DEALLOCATE (a); DEALLOCATE (ans)
END PROCEDURE elemsd_GetSUPGParam2

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_c(obj, tau, c, val, nu, k, &
  & phi, dt, opt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  !! element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilizing parameters
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  REAL(DFP), INTENT(IN) :: nu
  !! diffusivity coefficient
  REAL(DFP), OPTIONAL, INTENT(IN) :: k
  !! permeability
  REAL(DFP), OPTIONAL, INTENT(IN) :: phi
  !! porosity
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time step size
  !! default value is zero
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! default = 1
  !! opt=1
  !! opt=2
  !
  !!
  !! internal variables
  !!
  INTEGER(I4B) :: ii, opt0
  REAL(DFP) :: t1, t2, t3, t4, kbar, phibar
  REAL(DFP), ALLOCATABLE :: p(:, :)
  !! cdNdXt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! unit normal
  REAL(DFP), ALLOCATABLE :: q(:, :)
  !! rdNdXt
  REAL(DFP), ALLOCATABLE :: ans(:)
  !! result
  !! value of nu at space quadrature points
  TYPE(FEVariable_) :: rvar
  !! vector variable for keeping r
  !!
  !! MAIN
  !!
  opt0 = INPUT(default=1_I4B, option=opt)
  !!
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=p, val=c)
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
  !!
  IF (PRESENT(k)) THEN
    kbar = k
    phibar = phi
  ELSE
    kbar = MaxDFP
    phibar = 1.0_DFP
  END IF
  !!
  t2 = 0.0_DFP
  IF (PRESENT(dt)) THEN
    t2 = 2.0_DFP / dt
  END IF
  !!
  CALL Reallocate(ans, SIZE(obj%N, 2))
  !!
  IF (opt0 .EQ. 1_I4B) THEN
    DO ii = 1, SIZE(ans)
      t1 = SUM(ABS(p(:, ii)))
      t3 = nu * (SUM(ABS(q(:, ii))))**2
      t4 = 2.0_DFP * phibar * nu / kbar
      ans(ii) = SQRT(1.0_DFP / (t1**2 + t2**2 + t3**2 + t4**2))
    END DO
  ELSE
    DO ii = 1, SIZE(ans)
      t1 = SUM(ABS(p(:, ii)))
      t3 = nu * (SUM(ABS(q(:, ii))))**2
      t4 = 2.0_DFP * phibar * nu / kbar
      ans(ii) = 1.0_DFP / (t1 + t2 + t3 + t4)
    END DO
  END IF
  !!
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
  !! cleanup
  DEALLOCATE (p, r, q, ans)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_c

!----------------------------------------------------------------------------
!                                                              getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_d(obj, tau, c, val, nu, k, &
  & phi, dt, opt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  !! space-time element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilization parameter
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  !! scalar/vector variable
  REAL(DFP), INTENT(IN) :: nu
  !! diffusivity
  REAL(DFP), OPTIONAL, INTENT(IN) :: k
  !! permeability
  REAL(DFP), OPTIONAL, INTENT(IN) :: phi
  !! porosity
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time-step size
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! default=1
  !! opt=1,
  !! opt=2
  !!
  !! INTERNAL VARIABLES
  !!
  INTEGER(I4B) :: ii, opt0
  REAL(DFP) :: t12, t3, t4, kbar, phibar
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  !! cdNTdxt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! vector at space quad points
  REAL(DFP), ALLOCATABLE :: q(:, :, :)
  !!
  REAL(DFP), ALLOCATABLE :: ans(:)
  TYPE(FEVariable_) :: rvar
  !!
  !! MAIN
  !!
  opt0 = INPUT(default=1_I4B, option=opt)
  !!
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=p, val=c)
  !!
  !! make cdNTdxt + dNTdt
  !!
  p = p + obj%dNTdt
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=q, val=rvar)
  !!
  IF (PRESENT(k)) THEN
    kbar = k
    phibar = phi
  ELSE
    kbar = MaxDFP
    phibar = 1.0_DFP
  END IF
  !!
  !!
  CALL reallocate(ans, SIZE(obj%N, 2))
  !!
  IF (opt0 .EQ. 1_I4B) THEN
    DO ii = 1, SIZE(ans, 1)
      t12 = SUM(ABS(p(:, :, ii)))
      t3 = nu * (SUM(ABS(q(:, :, ii))))**2
      t4 = 2.0_DFP * phibar * nu / kbar
      ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))
    END DO
  ELSE
    DO ii = 1, SIZE(ans, 1)
      t12 = SUM(ABS(p(:, :, ii)))
      t3 = nu * (SUM(ABS(q(:, :, ii))))**2
      t4 = 2.0_DFP * phibar * nu / kbar
      ans(ii) = 1.0_DFP / (t12 + t3 + t4)
    END DO
  END IF
  !!
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
  !! cleanup
  !!
  DEALLOCATE (p, r, q, ans)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSUPGParam3
SELECT TYPE (obj)
TYPE IS (ElemshapeData_)
  !!
  CALL elemsd_getSUPGParam_c( &
    & obj=obj, &
    & tau=tau, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
  !!
CLASS IS (STElemshapeData_)
  !!
  CALL elemsd_getSUPGParam_d( &
    & obj=obj, &
    & tau=tau, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
  !!
END SELECT
END PROCEDURE elemsd_getSUPGParam3

!----------------------------------------------------------------------------
!                                                             GetSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUPGParam4
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: ans(:, :)
TYPE(FEVariable_) :: a
  !!
  !! main
  !!
CALL Reallocate(ans, SIZE(obj(1)%N, 2), SIZE(obj))
  !!
DO ii = 1, SIZE(obj)
    !!
  CALL elemsd_getSUPGParam_d( &
    & obj=obj(ii), &
    & tau=a, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & dt=dt, &
    & opt=opt)
    !!
  ans(:, ii) = Get(a, TypeFEVariableScalar, TypeFEVariableSpace)
    !!
END DO
  !!
tau = QuadratureVariable( &
  & ans, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
  !!
CALL DEALLOCATE (a)
DEALLOCATE (ans)
END PROCEDURE elemsd_GetSUPGParam4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE StabilizationParamMethods
