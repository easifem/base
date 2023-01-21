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

!!
!!
!! define internal variables
!!
INTEGER(I4B) :: ii
REAL(DFP) :: t1, t2, t3, t4, areal
REAL(DFP), ALLOCATABLE :: p(:, :)
!! cdNdXt
REAL(DFP), ALLOCATABLE :: r(:, :)
!! unit normal
REAL(DFP), ALLOCATABLE :: q(:, :)
!! rdNdXt
REAL(DFP), ALLOCATABLE :: tauvar(:), hvar(:), tausupsVar(:)
!! result
REAL(DFP), ALLOCATABLE :: nubar(:), kbar(:), phibar(:)
!! value of nu at space quadrature points
TYPE(FEVariable_) :: rvar, tausups
!! vector variable for keeping r
!!
!! Main
!!
CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=p, val=c)
!!
CALL GetUnitNormal(obj=obj, val=val, r=r)
rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
!!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
CALL GetInterpolation(obj=obj, val=k, interpol=kbar)
CALL GetInterpolation(obj=obj, val=phi, interpol=phibar)
!!
t2 = 0.0_DFP
IF (PRESENT(dt)) THEN
  IF (dt .GT. zero) t2 = 2.0_DFP / dt
END IF
!!
CALL Reallocate(tauvar, SIZE(obj%N, 2), hvar, SIZE(obj%N, 2), tausupsVar, &
  & SIZE(obj%N, 2))
!!
DO ii = 1, SIZE(tauvar)
  t1 = SUM(ABS(p(:, ii)))
  areal = SUM(ABS(q(:, ii)))
  hvar(ii) = 2.0_DFP / MAX(areal, smallDFP)
  t3 = nubar(ii) * areal**2
  t4 = (c4 * L0) * phibar(ii) * nubar(ii) / kbar(ii) / hvar(ii)
  tauvar(ii) = (1.0_DFP / (t1 + t2 + t3 + t4))
  tausupsVar(ii) = (1.0_DFP / (t1 + t2 + t3))
END DO
!!
tau = QuadratureVariable(tauvar, TypeFEVariableScalar, TypeFEVariableSpace)
tausups = QuadratureVariable(tausupsVar, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
!!
h = QuadratureVariable(hvar, TypeFEVariableScalar, TypeFEVariableSpace)
