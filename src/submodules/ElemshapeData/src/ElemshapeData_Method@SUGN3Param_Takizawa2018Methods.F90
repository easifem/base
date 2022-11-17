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

SUBMODULE(ElemshapeData_Method) SUGN3Param_Takizawa2018Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

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
!
!----------------------------------------------------------------------------

END SUBMODULE SUGN3Param_Takizawa2018Methods
