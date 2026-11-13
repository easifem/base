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
! summary: Strain measures
!
! Ported from the old `Strain` classes (Green, Almansi, Small),
! `DeformationGradient/StrainTensor.part` and
! `Old_StrainMeasures.part`.

SUBMODULE(Rank2Tensor_Method) StrainMethods
USE GlobalData, ONLY: Eye3
USE InvUtility, ONLY: Util_Inv => Inv
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                GreenStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GreenStrain1
ans%T = 0.5_DFP * (MATMUL(TRANSPOSE(F%T), F%T) - Eye3)
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_GreenStrain1

!----------------------------------------------------------------------------
!                                                                GreenStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GreenStrain2
ans%T = 0.5_DFP * (C%T - Eye3)
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_GreenStrain2

!----------------------------------------------------------------------------
!                                                        GreenStrain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GreenStrainPtr1
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=GreenStrain(F))
END PROCEDURE Rank2Tensor_GreenStrainPtr1

!----------------------------------------------------------------------------
!                                                        GreenStrain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GreenStrainPtr2
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=GreenStrain(C))
END PROCEDURE Rank2Tensor_GreenStrainPtr2

!----------------------------------------------------------------------------
!                                                              AlmansiStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AlmansiStrain1
REAL(DFP) :: invF(3, 3)

CALL Util_Inv(invA=invF, A=F%T)
ans%T = 0.5_DFP * (Eye3 - MATMUL(TRANSPOSE(invF), invF))
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_AlmansiStrain1

!----------------------------------------------------------------------------
!                                                              AlmansiStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AlmansiStrain2
REAL(DFP) :: invB(3, 3)

CALL Util_Inv(invA=invB, A=B%T)
ans%T = 0.5_DFP * (Eye3 - invB)
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_AlmansiStrain2

!----------------------------------------------------------------------------
!                                                      AlmansiStrain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AlmansiStrainPtr1
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=AlmansiStrain(F))
END PROCEDURE Rank2Tensor_AlmansiStrainPtr1

!----------------------------------------------------------------------------
!                                                      AlmansiStrain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AlmansiStrainPtr2
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=AlmansiStrain(B))
END PROCEDURE Rank2Tensor_AlmansiStrainPtr2

!----------------------------------------------------------------------------
!                                                                SmallStrain
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_SmallStrain
ans%T = 0.5_DFP * (gradU + TRANSPOSE(gradU))
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_SmallStrain

!----------------------------------------------------------------------------
!                                                        SmallStrain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_SmallStrainPtr
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=SmallStrain(gradU))
END PROCEDURE Rank2Tensor_SmallStrainPtr

END SUBMODULE StrainMethods
