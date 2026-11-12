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
! along with this program.  If not, see <https://www.gnu.org/licenses/>


!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress invariants
!
! Description:
!   Ported from the old `Stress/OperatorOverloading/Invariant.part` and
!   `Stress_Old/Invariants.part`. The old Haigh-Westergaard coordinate
!   `z` was computed as $\sigma_m/\sqrt{3}$; the correct value is
!   $I_1/\sqrt{3} = \sqrt{3}\sigma_m$.

SUBMODULE(Rank2Tensor_Method) StressInvarMethods
USE GlobalData, ONLY: CosineLode
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 MeanStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MeanStress
ans = Trace(obj) / 3.0_DFP
END PROCEDURE Rank2Tensor_MeanStress

!----------------------------------------------------------------------------
!                                                           EquivalentStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_EquivalentStress
ans = SQRT(3.0_DFP * MAX(J2(obj), 0.0_DFP))
END PROCEDURE Rank2Tensor_EquivalentStress

!----------------------------------------------------------------------------
!                                                           HaighWestergaard
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_HaighWestergaard
INTEGER(I4B) :: lodeType0

lodeType0 = Input(option=lodeType, default=CosineLode)
ans(1) = Trace(obj) / SQRT(3.0_DFP)
ans(2) = SQRT(2.0_DFP * MAX(J2(obj), 0.0_DFP))
ans(3) = LodeAngle(obj, lodeType0)
END PROCEDURE Rank2Tensor_HaighWestergaard

END SUBMODULE StressInvarMethods
