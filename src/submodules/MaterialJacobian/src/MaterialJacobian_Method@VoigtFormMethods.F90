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
! summary: Reduction of a 3D Voigt tangent to 2D Voigt forms
!
! Replaces the `Mat*_From_Mat*` functions of the old `Voigt` module,
! which is no longer available. Every form is a sub-matrix with the
! indices of GetVoigtIndex_ (VoigtUtility), except plane stress, which
! uses static condensation.
!
! Changes:
!   - 2026-11-17: indices from VoigtUtility, new forms (easifem/base#356)

SUBMODULE(MaterialJacobian_Method) VoigtFormMethods
USE GlobalData, ONLY: VoigtFormPlaneStress
USE InvUtility, ONLY: Util_Inv => Inv
USE VoigtUtility, ONLY: GetVoigtIndex_
IMPLICIT NONE

! In-plane and out-of-plane Voigt indices of plane stress
INTEGER(I4B), PARAMETER :: inPlane(3) = [1, 2, 4]
INTEGER(I4B), PARAMETER :: outPlane(3) = [3, 5, 6]

CONTAINS

!----------------------------------------------------------------------------
!                                                         ReduceVoigtTangent
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_ReduceVoigtTangent
INTEGER(I4B) :: indx(6), tsize

SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStress)
  ans = PlaneStressTangent(C)

CASE DEFAULT
  CALL GetVoigtIndex_(voigtForm=voigtForm, ans=indx, tsize=tsize)
  ans = C(indx(1:tsize), indx(1:tsize))
END SELECT
END PROCEDURE MaterialJacobian_ReduceVoigtTangent

!----------------------------------------------------------------------------
!                                                        ReduceVoigtTangent_
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_ReduceVoigtTangent_
INTEGER(I4B) :: indx(6), tsize

SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStress)
  nrow = 3
  ncol = 3
  ans(1:3, 1:3) = PlaneStressTangent(C)

CASE DEFAULT
  CALL GetVoigtIndex_(voigtForm=voigtForm, ans=indx, tsize=tsize)
  nrow = tsize
  ncol = tsize
  ans(1:tsize, 1:tsize) = C(indx(1:tsize), indx(1:tsize))
END SELECT
END PROCEDURE MaterialJacobian_ReduceVoigtTangent_

!----------------------------------------------------------------------------
!                                                         PlaneStressTangent
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Plane stress tangent by static condensation
!
! $C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$ with the in-plane set
! $i = [1, 2, 4]$ and the out-of-plane set $o = [3, 5, 6]$.

PURE FUNCTION PlaneStressTangent(C) RESULT(ans)
  REAL(DFP), INTENT(IN) :: C(6, 6)
  !! 3D Voigt tangent
  REAL(DFP) :: ans(3, 3)
  !! condensed tangent

  REAL(DFP) :: Coo(3, 3), invCoo(3, 3)

  Coo = C(outPlane, outPlane)
  CALL Util_Inv(invA=invCoo, A=Coo)
  ans = C(inPlane, inPlane) &
        - MATMUL(C(inPlane, outPlane), &
                 MATMUL(invCoo, C(outPlane, inPlane)))
END FUNCTION PlaneStressTangent

END SUBMODULE VoigtFormMethods
