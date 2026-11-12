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


!> Test: Kinematics
! summary: F, C, b, Green and Almansi strains, stretches, L, D, W

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: LeftCauchyGreen_
USE BaseType, ONLY: RightCauchyGreen_
USE BaseType, ONLY: GreenStrain_
USE BaseType, ONLY: AlmansiStrain_
USE BaseType, ONLY: SmallStrain_
USE BaseType, ONLY: VelocityGradient_
USE BaseType, ONLY: StrainRate_
USE BaseType, ONLY: ContinuumSpin_
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: DeformationGradient_Pointer
USE Rank2Tensor_Method, ONLY: Jacobian
USE Rank2Tensor_Method, ONLY: RightStretch
USE Rank2Tensor_Method, ONLY: LeftStretch
USE Rank2Tensor_Method, ONLY: Rotation
USE Rank2Tensor_Method, ONLY: PrincipalStretch
USE Rank2Tensor_Method, ONLY: PrincipalValues
USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
USE Rank2Tensor_Method, ONLY: RightCauchyGreen
USE Rank2Tensor_Method, ONLY: GreenStrain
USE Rank2Tensor_Method, ONLY: GreenStrain_Pointer
USE Rank2Tensor_Method, ONLY: AlmansiStrain
USE Rank2Tensor_Method, ONLY: SmallStrain
USE Rank2Tensor_Method, ONLY: VelocityGradient
USE Rank2Tensor_Method, ONLY: StrainRate
USE Rank2Tensor_Method, ONLY: ContinuumSpin
USE Rank2Tensor_Method, ONLY: Det
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(DeformationGradient_) :: F
CLASS(DeformationGradient_), POINTER :: pF
TYPE(LeftCauchyGreen_) :: b, b2
TYPE(RightCauchyGreen_) :: C, C2
TYPE(GreenStrain_) :: E, E2
CLASS(GreenStrain_), POINTER :: pE
TYPE(AlmansiStrain_) :: ea, ea2
TYPE(SmallStrain_) :: es
TYPE(VelocityGradient_) :: L
TYPE(StrainRate_) :: D
TYPE(ContinuumSpin_) :: W
TYPE(Rank2Tensor_) :: U, V, R
REAL(DFP) :: f0(3, 3), invF(3, 3), lam(3)

f0 = RESHAPE([1.2_DFP, 0.1_DFP, 0.0_DFP, &
              0.3_DFP, 0.9_DFP, 0.2_DFP, &
              0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
F = DeformationGradient(f0)
CALL OK(MAXVAL(ABS(F%T - f0)) .LT. tol, "DeformationGradient(mat)")
CALL OK(ABS(Jacobian(F) - Det(F)) .LT. tol, "Jacobian = det F")

pF => DeformationGradient_Pointer(f0)
CALL OK(MAXVAL(ABS(pF%T - f0)) .LT. tol, "DeformationGradient_Pointer")
DEALLOCATE (pF)

C = RightCauchyGreen(F=F)
b = LeftCauchyGreen(F=F)
CALL OK(C%isSym .AND. b%isSym, "C and b symmetric")
CALL OK(MAXVAL(ABS(C%T - MATMUL(TRANSPOSE(f0), f0))) .LT. tol, "C")
CALL OK(MAXVAL(ABS(b%T - MATMUL(f0, TRANSPOSE(f0)))) .LT. tol, "b")

U = RightStretch(F)
V = LeftStretch(F)
R = Rotation(F)
C2 = RightCauchyGreen(U=U)
b2 = LeftCauchyGreen(V=V)
CALL OK(MAXVAL(ABS(C2%T - C%T)) .LT. tol, "U^2 = C")
CALL OK(MAXVAL(ABS(b2%T - b%T)) .LT. tol, "V^2 = b")
CALL OK(MAXVAL(ABS(MATMUL(R%T, U%T) - f0)) .LT. tol, "R U = F")

lam = PrincipalStretch(F)
CALL OK(MAXVAL(ABS(lam - PrincipalValues(U))) .LT. 1.0E-8_DFP, &
        "PrincipalStretch = eig(U)")

E = GreenStrain(F)
CALL OK(MAXVAL(ABS(E%T - 0.5_DFP * (C%T - Eye3))) .LT. tol, "E")
E2 = GreenStrain(C)
CALL OK(MAXVAL(ABS(E2%T - E%T)) .LT. tol, "E(C) = E(F)")
pE => GreenStrain_Pointer(F)
CALL OK(MAXVAL(ABS(pE%T - E%T)) .LT. tol, "GreenStrain_Pointer")
DEALLOCATE (pE)

! Almansi strain is the push forward of E: e = F^{-T} E F^{-1}
ea = AlmansiStrain(F)
invF = f0
CALL InvertF(invF)
CALL OK(MAXVAL(ABS(ea%T - MATMUL(TRANSPOSE(invF), MATMUL(E%T, invF)))) &
        .LT. tol, "e = F^-T E F^-1")
ea2 = AlmansiStrain(b)
CALL OK(MAXVAL(ABS(ea2%T - ea%T)) .LT. tol, "e(b) = e(F)")

es = SmallStrain(f0 - Eye3)
CALL OK(MAXVAL(ABS(es%T - 0.5_DFP * (f0 + TRANSPOSE(f0)) + Eye3)) &
        .LT. tol, "SmallStrain")

L = VelocityGradient(f0)
D = StrainRate(L)
W = ContinuumSpin(L)
CALL OK(D%isSym .AND. .NOT. W%isSym, "D and W flags")
CALL OK(MAXVAL(ABS(D%T + W%T - f0)) .LT. tol, "D + W = L")
CALL OK(MAXVAL(ABS(W%T + TRANSPOSE(W%T))) .LT. tol, "W is skew")

CONTAINS

SUBROUTINE InvertF(A)
  REAL(DFP), INTENT(INOUT) :: A(3, 3)
  REAL(DFP) :: B(3, 3), d

  B(1, 1) = A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)
  B(1, 2) = A(1, 3) * A(3, 2) - A(1, 2) * A(3, 3)
  B(1, 3) = A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)
  B(2, 1) = A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)
  B(2, 2) = A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1)
  B(2, 3) = A(1, 3) * A(2, 1) - A(1, 1) * A(2, 3)
  B(3, 1) = A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1)
  B(3, 2) = A(1, 2) * A(3, 1) - A(1, 1) * A(3, 2)
  B(3, 3) = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
  d = A(1, 1) * B(1, 1) + A(1, 2) * B(2, 1) + A(1, 3) * B(3, 1)
  A = B / d
END SUBROUTINE InvertF

END PROGRAM main
