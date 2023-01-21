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

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

INTEGER(I4B) :: ii
REAL(DFP) :: t12, t3, t4, areal
REAL(DFP), ALLOCATABLE :: p(:, :, :)
!! cdNTdxt
REAL(DFP), ALLOCATABLE :: r(:, :)
!! vector at space quad points
REAL(DFP), ALLOCATABLE :: q(:, :, :)
!!
REAL(DFP), ALLOCATABLE :: tauvar(:), hvar(:), tausupsVar(:)
REAL(DFP), ALLOCATABLE :: nubar(:)
REAL(DFP), ALLOCATABLE :: kbar(:)
REAL(DFP), ALLOCATABLE :: phibar(:)
TYPE(FEVariable_) :: rvar, tausups
!!
!! main
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
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
CALL GetInterpolation(obj=obj, val=k, interpol=kbar)
CALL GetInterpolation(obj=obj, val=phi, interpol=phibar)
!!
CALL Reallocate(tauvar, SIZE(obj%N, 2), hvar, SIZE(obj%N, 2), tausupsVar, &
  & SIZE(obj%N, 2))
!!
DO ii = 1, SIZE(tauvar, 1)
  t12 = SUM(ABS(p(:, :, ii)))
  areal = (SUM(ABS(q(:, :, ii))))
  t3 = nubar(ii) * areal**2
  t4 = 2.0_DFP * phibar(ii) * nubar(ii) / kbar(ii)
  tauvar(ii) = (1.0_DFP / (t12 + t3 + t4))
  tausupsVar(ii) = (1.0_DFP / (t12 + t3))
  hvar(ii) = 2.0_DFP / MAX(areal, smallDFP)
END DO
!!
tau = QuadratureVariable(tauvar, TypeFEVariableScalar, TypeFEVariableSpace)
tausups = QuadratureVariable(tausupsVar, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
h = QuadratureVariable(hvar, TypeFEVariableScalar, TypeFEVariableSpace)
