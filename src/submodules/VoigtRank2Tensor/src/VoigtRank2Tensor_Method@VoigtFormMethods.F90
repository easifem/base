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
! Ported from the old `Stress/OperatorOverloading/Shape.part` and the
! NSD logic of the old `Stress_Class` constructor.
!
! Changes:
!   - 2026-11-17: the index orders come from VoigtUtility (one table for
!     all forms), added GetVoigtSize, easifem/base#356

SUBMODULE(VoigtRank2Tensor_Method) VoigtFormMethods
USE VoigtUtility, ONLY: Array_GetVoigtVector => GetVoigtVector
USE VoigtUtility, ONLY: Array_GetVoigtVector_ => GetVoigtVector_
USE VoigtUtility, ONLY: VoigtFromForm
USE VoigtUtility, ONLY: VoigtSize
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               GetVoigtSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVoigtSize
ans = VoigtSize(voigtForm)
END PROCEDURE obj_GetVoigtSize

!----------------------------------------------------------------------------
!                                                             GetVoigtVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVoigtVector
ans = Array_GetVoigtVector(v=obj%V, voigtForm=voigtForm)
END PROCEDURE obj_GetVoigtVector

!----------------------------------------------------------------------------
!                                                            GetVoigtVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVoigtVector_
CALL Array_GetVoigtVector_(v=obj%V, voigtForm=voigtForm, ans=ans, &
                           tsize=tsize)
END PROCEDURE obj_GetVoigtVector_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
REAL(DFP) :: v6(6)

CALL VoigtFromForm(vec=vec, voigtForm=voigtForm, ans=v6)
CALL Initiate(obj=obj, vec=v6, voigtType=voigtType)
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                           VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_VoigtRank2Tensor3
CALL Initiate(obj=ans, vec=vec, voigtType=voigtType, voigtForm=voigtForm)
END PROCEDURE obj_VoigtRank2Tensor3

END SUBMODULE VoigtFormMethods
