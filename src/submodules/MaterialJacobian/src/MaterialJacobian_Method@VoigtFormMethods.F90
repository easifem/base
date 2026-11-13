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
! which is no longer available. A plain sub-matrix is only correct for
! plane strain and axisymmetry; plane stress uses static condensation.

SUBMODULE(MaterialJacobian_Method) VoigtFormMethods
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE InvUtility, ONLY: Util_Inv => Inv
IMPLICIT NONE

! In-plane and out-of-plane Voigt indices of the 2D forms
INTEGER(I4B), PARAMETER :: inPlane(3) = [1, 2, 4]
INTEGER(I4B), PARAMETER :: outPlane(3) = [3, 5, 6]
INTEGER(I4B), PARAMETER :: axisym(4) = [1, 2, 4, 3]

CONTAINS

!----------------------------------------------------------------------------
!                                                         ReduceVoigtTangent
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_ReduceVoigtTangent
REAL(DFP) :: Coo(3, 3), invCoo(3, 3)

SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain)
  ans = C(inPlane, inPlane)

CASE (VoigtFormAxisymmetric)
  ans = C(axisym, axisym)

CASE (VoigtFormPlaneStress)
  Coo = C(outPlane, outPlane)
  CALL Util_Inv(invA=invCoo, A=Coo)
  ans = C(inPlane, inPlane) &
        - MATMUL(C(inPlane, outPlane), &
                 MATMUL(invCoo, C(outPlane, inPlane)))

CASE DEFAULT
  ans = C
END SELECT
END PROCEDURE MaterialJacobian_ReduceVoigtTangent

!----------------------------------------------------------------------------
!                                                        ReduceVoigtTangent_
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_ReduceVoigtTangent_
REAL(DFP) :: Coo(3, 3), invCoo(3, 3)

SELECT CASE (voigtForm)
CASE (VoigtFormPlaneStrain)
  nrow = 3
  ncol = 3
  ans(1:3, 1:3) = C(inPlane, inPlane)

CASE (VoigtFormAxisymmetric)
  nrow = 4
  ncol = 4
  ans(1:4, 1:4) = C(axisym, axisym)

CASE (VoigtFormPlaneStress)
  nrow = 3
  ncol = 3
  Coo = C(outPlane, outPlane)
  CALL Util_Inv(invA=invCoo, A=Coo)
  ans(1:3, 1:3) = C(inPlane, inPlane) &
                  - MATMUL(C(inPlane, outPlane), &
                           MATMUL(invCoo, C(outPlane, inPlane)))

CASE DEFAULT
  nrow = 6
  ncol = 6
  ans(1:6, 1:6) = C
END SELECT
END PROCEDURE MaterialJacobian_ReduceVoigtTangent_

END SUBMODULE VoigtFormMethods
