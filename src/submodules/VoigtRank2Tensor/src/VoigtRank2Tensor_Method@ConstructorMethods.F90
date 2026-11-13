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
! date: 2021-03-13
! summary: Constructor and assignment methods of [[VoigtRank2Tensor_]]

SUBMODULE(VoigtRank2Tensor_Method) ConstructorMethods
USE GlobalData, ONLY: StrainTypeVoigt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Initiate1
obj%voigtType = voigtType
obj%V = vec
obj%scale = GetScale(voigtType)
END PROCEDURE VoigtRank2Tensor_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Initiate2
REAL(DFP) :: s

obj%voigtType = voigtType
obj%scale = GetScale(voigtType)

! s is 1 for stress-like and 2 for strain-like (engineering shear)
s = 1.0_DFP / obj%scale

obj%V(1) = T(1, 1)
obj%V(2) = T(2, 2)
obj%V(3) = T(3, 3)
obj%V(4) = 0.5_DFP * s * (T(1, 2) + T(2, 1))
obj%V(5) = 0.5_DFP * s * (T(2, 3) + T(3, 2))
obj%V(6) = 0.5_DFP * s * (T(1, 3) + T(3, 1))
END PROCEDURE VoigtRank2Tensor_Initiate2

!----------------------------------------------------------------------------
!                                                           VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Constructor1
CALL Initiate(obj=ans, vec=vec, voigtType=voigtType)
END PROCEDURE VoigtRank2Tensor_Constructor1

!----------------------------------------------------------------------------
!                                                           VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Constructor2
CALL Initiate(obj=ans, T=T, voigtType=voigtType)
END PROCEDURE VoigtRank2Tensor_Constructor2

!----------------------------------------------------------------------------
!                                                   VoigtRank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Pointer1
ALLOCATE (ans)
CALL Initiate(obj=ans, vec=vec, voigtType=voigtType)
END PROCEDURE VoigtRank2Tensor_Pointer1

!----------------------------------------------------------------------------
!                                                   VoigtRank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Pointer2
ALLOCATE (ans)
CALL Initiate(obj=ans, T=T, voigtType=voigtType)
END PROCEDURE VoigtRank2Tensor_Pointer2

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_MatEqObj
T(1, 1) = obj%V(1)
T(2, 2) = obj%V(2)
T(3, 3) = obj%V(3)
T(1, 2) = obj%scale * obj%V(4)
T(2, 1) = T(1, 2)
T(2, 3) = obj%scale * obj%V(5)
T(3, 2) = T(2, 3)
T(1, 3) = obj%scale * obj%V(6)
T(3, 1) = T(1, 3)
END PROCEDURE VoigtRank2Tensor_MatEqObj

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_VecEqObj
vec = obj%V
END PROCEDURE VoigtRank2Tensor_VecEqObj

!----------------------------------------------------------------------------
!                                                                   GetScale
!----------------------------------------------------------------------------

! Returns the shear scale factor for a Voigt type. Strain-like gives 0.5,
! anything else is treated as stress-like and gives 1.0.

PURE FUNCTION GetScale(voigtType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: voigtType
  REAL(DFP) :: ans

  SELECT CASE (voigtType)
  CASE (StrainTypeVoigt)
    ans = 0.5_DFP
  CASE DEFAULT
    ans = 1.0_DFP
  END SELECT
END FUNCTION GetScale

END SUBMODULE ConstructorMethods
