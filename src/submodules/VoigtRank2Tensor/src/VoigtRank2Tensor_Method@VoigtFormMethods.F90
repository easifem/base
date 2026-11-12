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
! summary: Reduced (2D) Voigt forms of [[VoigtRank2Tensor_]]
!
! Description:
!   Ported from the old `Stress/OperatorOverloading/Shape.part` and the
!   NSD logic of the old `Stress_Class` constructor.

SUBMODULE(VoigtRank2Tensor_Method) VoigtFormMethods
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormAxisymmetric
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  VoigtSize
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_VoigtSize
SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain, VoigtFormPlaneStress)
  ans = 3
CASE (VoigtFormAxisymmetric)
  ans = 4
CASE DEFAULT
  ans = 6
END SELECT
END PROCEDURE VoigtRank2Tensor_VoigtSize

!----------------------------------------------------------------------------
!                                                             GetVoigtVector
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_GetVoigtVector
SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain, VoigtFormPlaneStress)
  ans = [obj%V(1), obj%V(2), obj%V(4)]
CASE (VoigtFormAxisymmetric)
  ans = [obj%V(1), obj%V(2), obj%V(4), obj%V(3)]
CASE DEFAULT
  ans = obj%V
END SELECT
END PROCEDURE VoigtRank2Tensor_GetVoigtVector

!----------------------------------------------------------------------------
!                                                            GetVoigtVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_GetVoigtVector_
SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain, VoigtFormPlaneStress)
  tsize = 3
  ans(1) = obj%V(1)
  ans(2) = obj%V(2)
  ans(3) = obj%V(4)
CASE (VoigtFormAxisymmetric)
  tsize = 4
  ans(1) = obj%V(1)
  ans(2) = obj%V(2)
  ans(3) = obj%V(4)
  ans(4) = obj%V(3)
CASE DEFAULT
  tsize = 6
  ans(1:6) = obj%V
END SELECT
END PROCEDURE VoigtRank2Tensor_GetVoigtVector_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Initiate3
REAL(DFP) :: v6(6)

v6 = 0.0_DFP

SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain, VoigtFormPlaneStress)
  v6(1) = vec(1)
  v6(2) = vec(2)
  v6(4) = vec(3)
CASE (VoigtFormAxisymmetric)
  v6(1) = vec(1)
  v6(2) = vec(2)
  v6(4) = vec(3)
  v6(3) = vec(4)
CASE DEFAULT
  v6 = vec(1:6)
END SELECT

CALL Initiate(obj=obj, vec=v6, voigtType=voigtType)
END PROCEDURE VoigtRank2Tensor_Initiate3

!----------------------------------------------------------------------------
!                                                           VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Constructor3
CALL Initiate(obj=ans, vec=vec, voigtType=voigtType, voigtForm=voigtForm)
END PROCEDURE VoigtRank2Tensor_Constructor3

END SUBMODULE VoigtFormMethods
