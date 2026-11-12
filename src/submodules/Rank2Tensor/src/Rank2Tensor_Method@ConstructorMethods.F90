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

SUBMODULE(Rank2Tensor_Method) ConstructorMethods
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: StressTypeVoigt
USE InputUtility, ONLY: Input
USE InvUtility, ONLY: Util_Inv => Inv
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor_Initiate => Initiate
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Initiate1
obj%T = obj2%T
obj%isSym = obj2%isSym

! Keep the stress measure when a Stress_ is copied into a Stress_
SELECT TYPE (obj)
CLASS IS (Stress_)
  SELECT TYPE (obj2)
  CLASS IS (Stress_)
    obj%stressType = obj2%stressType
  END SELECT
END SELECT
END PROCEDURE Rank2Tensor_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Initiate2
obj%T = mat
obj%isSym = Input(option=isSym, default=.FALSE.)
END PROCEDURE Rank2Tensor_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Initiate3
obj%T(1, 1) = V%V(1)
obj%T(2, 2) = V%V(2)
obj%T(3, 3) = V%V(3)
obj%T(1, 2) = V%scale * V%V(4)
obj%T(2, 1) = obj%T(1, 2)
obj%T(2, 3) = V%scale * V%V(5)
obj%T(3, 2) = obj%T(2, 3)
obj%T(1, 3) = V%scale * V%V(6)
obj%T(3, 1) = obj%T(1, 3)
obj%isSym = .TRUE.
END PROCEDURE Rank2Tensor_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Initiate4
CALL VoigtRank2Tensor_Initiate(obj=obj, T=T%T, voigtType=voigtType)
END PROCEDURE Rank2Tensor_Initiate4

!----------------------------------------------------------------------------
!                                                                Rank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Constructor1
CALL Initiate(obj=ans, mat=mat, isSym=isSym)
END PROCEDURE Rank2Tensor_Constructor1

!----------------------------------------------------------------------------
!                                                                Rank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Constructor2
CALL Initiate(obj=ans, V=V)
END PROCEDURE Rank2Tensor_Constructor2

!----------------------------------------------------------------------------
!                                                        Rank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Pointer1
ALLOCATE (ans)
CALL Initiate(obj=ans, mat=mat, isSym=isSym)
END PROCEDURE Rank2Tensor_Pointer1

!----------------------------------------------------------------------------
!                                                        Rank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Pointer2
ALLOCATE (ans)
CALL Initiate(obj=ans, V=V)
END PROCEDURE Rank2Tensor_Pointer2

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_ObjEqMat
CALL Initiate(obj=obj, mat=mat)
END PROCEDURE Rank2Tensor_ObjEqMat

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MatEqObj
mat = obj%T
END PROCEDURE Rank2Tensor_MatEqObj

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_VoigtEqObj
CALL VoigtRank2Tensor_Initiate(obj=V, T=obj%T, &
                               voigtType=StressTypeVoigt)
END PROCEDURE Rank2Tensor_VoigtEqObj

!----------------------------------------------------------------------------
!                                                             IdentityTensor
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_IdentityTensor
CALL Initiate(obj=obj, mat=Eye3, isSym=.TRUE.)
END PROCEDURE Rank2Tensor_IdentityTensor

!----------------------------------------------------------------------------
!                                                                    getOnes
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GetOnes
obj%T = 1.0_DFP
obj%isSym = .TRUE.
END PROCEDURE Rank2Tensor_GetOnes

!----------------------------------------------------------------------------
!                                                                   getZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GetZeros
obj%T = 0.0_DFP
obj%isSym = .TRUE.
END PROCEDURE Rank2Tensor_GetZeros

!----------------------------------------------------------------------------
!                                                            IsotropicTensor
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_IsotropicTensor
CALL Initiate(obj=obj, mat=lambda * Eye3, isSym=.TRUE.)
END PROCEDURE Rank2Tensor_IsotropicTensor

!----------------------------------------------------------------------------
!                                                                      isSym
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_IsSym
ans = obj%isSym
END PROCEDURE Rank2Tensor_IsSym

!----------------------------------------------------------------------------
!                                                               isDeviatoric
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_IsDeviatoric
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: tr

tr = obj%T(1, 1) + obj%T(2, 2) + obj%T(3, 3)
ans = ABS(tr) .LE. tol * MAXVAL(ABS(obj%T))
END PROCEDURE Rank2Tensor_IsDeviatoric

!----------------------------------------------------------------------------
!                                                                        Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Inv
CALL Util_Inv(invA=invObj%T, A=obj%T)
invObj%isSym = obj%isSym
END PROCEDURE Rank2Tensor_Inv

!----------------------------------------------------------------------------
!                                                                  Transpose
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Transpose
ans%T = TRANSPOSE(obj%T)
ans%isSym = obj%isSym
END PROCEDURE Rank2Tensor_Transpose

!----------------------------------------------------------------------------
!                                                                        Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Sym
IF (obj%isSym) THEN
  ans%T = obj%T
ELSE
  ans%T = 0.5_DFP * (obj%T + TRANSPOSE(obj%T))
END IF
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_Sym

!----------------------------------------------------------------------------
!                                                                    SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_SkewSym
ans%T = 0.5_DFP * (obj%T - TRANSPOSE(obj%T))
ans%isSym = .FALSE.
END PROCEDURE Rank2Tensor_SkewSym

END SUBMODULE ConstructorMethods
