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
! summary: Derivatives of stress invariants with respect to stress
!
! Ported from the old `Stress_Old/StressDerivatives.part` and
! `Stress_Old/getHillTensor.part`. The results are symmetric tensors.
! The old optional `Voigt` flag doubled the shear entries, which is
! `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.

SUBMODULE(Rank2Tensor_Method) StressDerivativeMethods
USE GlobalData, ONLY: Eye3
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       MeanStressDerivative
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MeanStressDerivative
! The derivative does not depend on the stress, obj only fixes the API
ans%T = Eye3 / 3.0_DFP
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_MeanStressDerivative

!----------------------------------------------------------------------------
!                                                               J2Derivative
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_J2Derivative
ans = Deviatoric(obj)
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_J2Derivative

!----------------------------------------------------------------------------
!                                                               J3Derivative
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_J3Derivative
TYPE(Rank2Tensor_) :: s

s = Deviatoric(obj)
ans%T = MATMUL(s%T, s%T) - (2.0_DFP / 3.0_DFP) * J2(s, .TRUE.) * Eye3
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_J3Derivative

!----------------------------------------------------------------------------
!                                                 EquivalentStressDerivative
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_EquivalentStressDeriv
REAL(DFP) :: seq

seq = EquivalentStress(obj)
ans%isSym = .TRUE.

IF (seq .LE. TINY(1.0_DFP)) THEN
  ans%T = 0.0_DFP
  RETURN
END IF

ans = Deviatoric(obj)
ans%T = (1.5_DFP / seq) * ans%T
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_EquivalentStressDeriv

END SUBMODULE StressDerivativeMethods
