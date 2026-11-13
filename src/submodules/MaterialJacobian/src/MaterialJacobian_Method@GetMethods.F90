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
! summary: Getters of [[MaterialJacobian_]] (tags, components)
!
! Ported from the old `MaterialJacobian/getCijkl.part` and
! `OperatorOverloading/Cijkl.part` (`.Cijkl.` and `.Shape.`).

SUBMODULE(MaterialJacobian_Method) GetMethods
USE GlobalData, ONLY: VoigtForm3D
USE InputUtility, ONLY: Input
IMPLICIT NONE

! Voigt index of the tensor index pair (i, j)
INTEGER(I4B), PARAMETER :: voigtIndex(3, 3) = RESHAPE( &
                           [1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])

CONTAINS

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
!                                                                      Cijkl
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Cijkl
ans = obj%C(voigtIndex(i, j), voigtIndex(k, l))
END PROCEDURE MaterialJacobian_Cijkl

!----------------------------------------------------------------------------
!                                                                   GetRank4
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_GetRank4
INTEGER(I4B) :: i, j, k, l

DO l = 1, 3
  DO k = 1, 3
    DO j = 1, 3
      DO i = 1, 3
        ans(i, j, k, l) = obj%C(voigtIndex(i, j), voigtIndex(k, l))
      END DO
    END DO
  END DO
END DO
END PROCEDURE MaterialJacobian_GetRank4

!----------------------------------------------------------------------------
!                                                                 GetTangent
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_GetTangent
ans = ReduceVoigtTangent(C=obj%C, &
                         voigtForm=Input(option=voigtForm, &
                                         default=VoigtForm3D))
END PROCEDURE MaterialJacobian_GetTangent

END SUBMODULE GetMethods
