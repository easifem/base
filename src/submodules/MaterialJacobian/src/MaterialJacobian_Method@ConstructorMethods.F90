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
! summary: Constructors and measure tags of [[MaterialJacobian_]]
!
! Description:
!   Ported from the old `MaterialJacobian/Initiate.part`,
!   `MaterialJacobian.part`, `MaterialJacobian_Pointer.part` and
!   `Names.part`. The old free-form string tags are replaced by the
!   `StressMeasure*`, `StrainMeasure*` and `StressRate*` constants.

SUBMODULE(MaterialJacobian_Method) ConstructorMethods
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StressRateNone
USE InputUtility, ONLY: Input
IMPLICIT NONE

! Tensor index pair (voigtRow(I), voigtCol(I)) of the Voigt index I
INTEGER(I4B), PARAMETER :: voigtRow(6) = [1, 2, 3, 1, 2, 1]
INTEGER(I4B), PARAMETER :: voigtCol(6) = [1, 2, 3, 2, 3, 3]

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Initiate1
obj%C = C
obj%stressType = Input(option=stressType, default=StressMeasureCauchy)
obj%strainType = Input(option=strainType, default=StrainMeasureSmall)
obj%rateType = Input(option=rateType, default=StressRateNone)
END PROCEDURE MaterialJacobian_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Initiate2
REAL(DFP) :: C(6, 6)
INTEGER(I4B) :: II, JJ, i, j, k, l

DO JJ = 1, 6
  k = voigtRow(JJ)
  l = voigtCol(JJ)
  DO II = 1, 6
    i = voigtRow(II)
    j = voigtCol(II)
    C(II, JJ) = 0.25_DFP * (C4(i, j, k, l) + C4(j, i, k, l) &
                            + C4(i, j, l, k) + C4(j, i, l, k))
  END DO
END DO

CALL Initiate(obj=obj, C=C, stressType=stressType, &
              strainType=strainType, rateType=rateType)
END PROCEDURE MaterialJacobian_Initiate2

!----------------------------------------------------------------------------
!                                                           MaterialJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Constructor1
CALL Initiate(obj=ans, C=C, stressType=stressType, &
              strainType=strainType, rateType=rateType)
END PROCEDURE MaterialJacobian_Constructor1

!----------------------------------------------------------------------------
!                                                           MaterialJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Constructor2
CALL Initiate(obj=ans, C4=C4, stressType=stressType, &
              strainType=strainType, rateType=rateType)
END PROCEDURE MaterialJacobian_Constructor2

!----------------------------------------------------------------------------
!                                                   MaterialJacobian_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Pointer1
ALLOCATE (ans)
CALL Initiate(obj=ans, C=C, stressType=stressType, &
              strainType=strainType, rateType=rateType)
END PROCEDURE MaterialJacobian_Pointer1

!----------------------------------------------------------------------------
!                                                   MaterialJacobian_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Pointer2
ALLOCATE (ans)
CALL Initiate(obj=ans, C4=C4, stressType=stressType, &
              strainType=strainType, rateType=rateType)
END PROCEDURE MaterialJacobian_Pointer2

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_MatEqObj
C = obj%C
END PROCEDURE MaterialJacobian_MatEqObj

!----------------------------------------------------------------------------
!                                                              GetStressType
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_GetStressType
ans = obj%stressType
END PROCEDURE MaterialJacobian_GetStressType

!----------------------------------------------------------------------------
!                                                              GetStrainType
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_GetStrainType
ans = obj%strainType
END PROCEDURE MaterialJacobian_GetStrainType

!----------------------------------------------------------------------------
!                                                                GetRateType
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_GetRateType
ans = obj%rateType
END PROCEDURE MaterialJacobian_GetRateType

!----------------------------------------------------------------------------
!                                                            SetMeasureTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_SetMeasureTypes
IF (PRESENT(stressType)) obj%stressType = stressType
IF (PRESENT(strainType)) obj%strainType = strainType
IF (PRESENT(rateType)) obj%rateType = rateType
END PROCEDURE MaterialJacobian_SetMeasureTypes

END SUBMODULE ConstructorMethods
