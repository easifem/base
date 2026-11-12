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


!> Test: MaterialJacobian contraction and rank-4 builders
! summary: C : eps for isotropic tangent, Isym : eps, Pdev : eps

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: StrainTypeVoigt
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lam = 2.0_DFP, mu = 3.0_DFP
TYPE(MaterialJacobian_) :: obj
TYPE(Rank2Tensor_) :: eps, sig, D
TYPE(VoigtRank2Tensor_) :: veps, vsig
REAL(DFP) :: e0(3, 3), expected(3, 3), T(3, 3)
INTEGER(I4B) :: k, types(2)

e0 = RESHAPE([1.0_DFP, 0.2_DFP, -0.3_DFP, &
              0.2_DFP, 2.0_DFP, 0.4_DFP, &
              -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
expected = lam * (e0(1, 1) + e0(2, 2) + e0(3, 3)) * Eye3 + 2.0_DFP * mu * e0

obj = MaterialJacobian(IsotropicTangent(lam, mu))
eps = Rank2Tensor(e0, .TRUE.)
sig = Contraction(obj, eps)
CALL OK(MAXVAL(ABS(sig%T - expected)) .LT. tol .AND. sig%isSym, &
        "isotropic: C : eps (Rank2Tensor)")

types = [StressTypeVoigt, StrainTypeVoigt]
DO k = 1, 2
  veps = VoigtRank2Tensor(e0, types(k))
  vsig = Contraction(obj, veps)
  T = vsig
  CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "isotropic: C : eps (Voigt)")
END DO

obj = MaterialJacobian(Rank4SymIdentity())
sig = Contraction(obj, eps)
CALL OK(MAXVAL(ABS(sig%T - e0)) .LT. tol, "Isym : eps = eps")

obj = MaterialJacobian(Rank4DeviatoricProjector())
sig = Contraction(obj, eps)
D = Dev(eps)
CALL OK(MAXVAL(ABS(sig%T - D%T)) .LT. tol, "Pdev : eps = dev eps")

END PROGRAM main
