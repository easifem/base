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
! summary: Contraction of [[MaterialJacobian_]] with a strain
!
! Description:
!   Ported from the old `MaterialJacobian/OperatorOverloading/
!   Contraction.part`. The strain is converted to the engineering
!   Voigt vector $[\varepsilon_{11}, \varepsilon_{22}, \varepsilon_{33},
!   \gamma_{12}, \gamma_{23}, \gamma_{13}]$ and multiplied by `C`.

SUBMODULE(MaterialJacobian_Method) ContractionMethods
USE GlobalData, ONLY: StressTypeVoigt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Contraction1
REAL(DFP) :: e(6), s(6)

! Engineering shear of the symmetric part: eps_ij + eps_ji
e(1) = eps%T(1, 1)
e(2) = eps%T(2, 2)
e(3) = eps%T(3, 3)
e(4) = eps%T(1, 2) + eps%T(2, 1)
e(5) = eps%T(2, 3) + eps%T(3, 2)
e(6) = eps%T(1, 3) + eps%T(3, 1)

s = MATMUL(obj%C, e)

ans%T(1, 1) = s(1)
ans%T(2, 2) = s(2)
ans%T(3, 3) = s(3)
ans%T(1, 2) = s(4)
ans%T(2, 1) = s(4)
ans%T(2, 3) = s(5)
ans%T(3, 2) = s(5)
ans%T(1, 3) = s(6)
ans%T(3, 1) = s(6)
ans%isSym = .TRUE.
END PROCEDURE MaterialJacobian_Contraction1

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Contraction2
REAL(DFP) :: e(6)

! The tensor shear component is scale * V, the engineering one twice that
e(1:3) = eps%V(1:3)
e(4:6) = 2.0_DFP * eps%scale * eps%V(4:6)

ans%V = MATMUL(obj%C, e)
ans%scale = 1.0_DFP
ans%voigtType = StressTypeVoigt
END PROCEDURE MaterialJacobian_Contraction2

END SUBMODULE ContractionMethods
