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
! summary: Velocity gradient, strain rate and spin
!
! Description:
!   Ported from the old `VelocityGradient_Class`, `StrainRate_Class`
!   and `ContinuumSpin_Class`.

SUBMODULE(Rank2Tensor_Method) VelocityGradientMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           VelocityGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_VelocityGradient
CALL Initiate(obj=ans, mat=mat)
END PROCEDURE Rank2Tensor_VelocityGradient

!----------------------------------------------------------------------------
!                                                   VelocityGradient_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_VelocityGradientPtr
ALLOCATE (ans)
CALL Initiate(obj=ans, mat=mat)
END PROCEDURE Rank2Tensor_VelocityGradientPtr

!----------------------------------------------------------------------------
!                                                                 StrainRate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_StrainRate
ans%T = 0.5_DFP * (L%T + TRANSPOSE(L%T))
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_StrainRate

!----------------------------------------------------------------------------
!                                                         StrainRate_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_StrainRatePtr
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=StrainRate(L))
END PROCEDURE Rank2Tensor_StrainRatePtr

!----------------------------------------------------------------------------
!                                                              ContinuumSpin
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_ContinuumSpin
ans%T = 0.5_DFP * (L%T - TRANSPOSE(L%T))
ans%isSym = .FALSE.
END PROCEDURE Rank2Tensor_ContinuumSpin

!----------------------------------------------------------------------------
!                                                      ContinuumSpin_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_ContinuumSpinPtr
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=ContinuumSpin(L))
END PROCEDURE Rank2Tensor_ContinuumSpinPtr

END SUBMODULE VelocityGradientMethods
