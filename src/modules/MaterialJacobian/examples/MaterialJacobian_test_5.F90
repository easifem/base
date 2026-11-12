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


!> Test: stress and strain at one quadrature point (elastodynamics)
! summary: given lambda, mu and grad(u), compute C, strain and stress
!
! In elastodynamics, M a + f_int(u) = f_ext. At every quadrature point
! and time step we need the stress for f_int = int B^T sigma dV and, for
! implicit schemes, the tangent C for K = int B^T C B dV. This example
! shows these steps with lambda = 2, mu = 1 and a simple grad(u).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: SmallStrain_
USE BaseType, ONLY: GreenStrain_
USE BaseType, ONLY: Stress_
USE Rank2Tensor_Method, ONLY: SmallStrain
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: GreenStrain
USE Rank2Tensor_Method, ONLY: Stress
USE Rank2Tensor_Method, ONLY: PK1Stress
USE Rank2Tensor_Method, ONLY: CauchyStress
USE Rank2Tensor_Method, ONLY: Jacobian
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: MeanStress
USE Rank2Tensor_Method, ONLY: Display
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: GetTangent
USE MaterialJacobian_Method, ONLY: Display
USE Display_Method, ONLY: Display
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lam = 2.0_DFP
REAL(DFP), PARAMETER :: mu = 1.0_DFP
REAL(DFP) :: gradU(3, 3), eps0(3, 3), sig0(3, 3), trEps
REAL(DFP) :: e6(6), s6(6)
REAL(DFP), ALLOCATABLE :: D(:, :), e2(:)
TYPE(MaterialJacobian_) :: C
TYPE(SmallStrain_) :: eps
TYPE(Rank2Tensor_) :: sig
TYPE(VoigtRank2Tensor_) :: epsV
TYPE(DeformationGradient_) :: F
TYPE(GreenStrain_) :: EE
TYPE(Stress_) :: S, P, cauchy

!----------------------------------------------------------------------------
! 1. Material tangent C = lambda I x I + 2 mu Isym (6x6 Voigt)
!----------------------------------------------------------------------------
!
!   | lam+2mu  lam      lam      0   0   0  |   | 4 2 2 0 0 0 |
!   | lam      lam+2mu  lam      0   0   0  |   | 2 4 2 0 0 0 |
!   | lam      lam      lam+2mu  0   0   0  | = | 2 2 4 0 0 0 |
!   | 0        0        0        mu  0   0  |   | 0 0 0 1 0 0 |
!   | 0        0        0        0   mu  0  |   | 0 0 0 0 1 0 |
!   | 0        0        0        0   0   mu |   | 0 0 0 0 0 1 |

C = MaterialJacobian(IsotropicTangent(lam, mu))
CALL Display(C, "1. C (6x6 Voigt):")
CALL OK(ABS(C%C(1, 1) - 4.0_DFP) .LT. tol .AND. &
        ABS(C%C(1, 2) - 2.0_DFP) .LT. tol .AND. &
        ABS(C%C(4, 4) - 1.0_DFP) .LT. tol, "C entries")

!----------------------------------------------------------------------------
! 2. Displacement gradient, grad(u)_ij = du_i / dx_j
!----------------------------------------------------------------------------
!
! In a FE code: gradU = MATMUL(uNodes, dNdX), uNodes(3, nns) are the
! nodal displacements and dNdX(nns, 3) the shape function gradients.
! Here: u1 = 0.01 x + 0.02 y, u2 = u3 = 0

gradU = 0.0_DFP
gradU(1, 1) = 0.01_DFP
gradU(1, 2) = 0.02_DFP
CALL Display(gradU, "2. grad(u):")

!----------------------------------------------------------------------------
! 3. Small strain eps = (grad u + grad u^T) / 2
!----------------------------------------------------------------------------

eps = SmallStrain(gradU)
CALL Display(eps, "3. eps:")
eps0 = 0.5_DFP * (gradU + TRANSPOSE(gradU))
CALL OK(MAXVAL(ABS(eps%T - eps0)) .LT. tol, "eps = sym(grad u)")

!----------------------------------------------------------------------------
! 4. Stress sigma = C : eps = lambda tr(eps) I + 2 mu eps
!----------------------------------------------------------------------------
!
! sigma11 = 2*0.01 + 2*0.01 = 0.04,  sigma22 = sigma33 = 0.02,
! sigma12 = 2*1*0.01 = 0.02

sig = Contraction(C, eps)
CALL Display(sig, "4. sigma:")
trEps = eps0(1, 1) + eps0(2, 2) + eps0(3, 3)
sig0 = lam * trEps * Eye3 + 2.0_DFP * mu * eps0
CALL OK(MAXVAL(ABS(sig%T - sig0)) .LT. tol, "sigma = C : eps")
WRITE (*, "(A, ES12.5)") "von Mises stress = ", EquivalentStress(sig)
WRITE (*, "(A, ES12.5)") "pressure         = ", -MeanStress(sig)

!----------------------------------------------------------------------------
! 5. The same in 3D Voigt form: s6 = C e6 with engineering shear
!----------------------------------------------------------------------------
!
! e6 = [e11, e22, e33, 2 e12, 2 e23, 2 e13], this is what B u gives

epsV = VoigtRank2Tensor(eps%T, StrainTypeVoigt)
e6 = epsV%V
s6 = MATMUL(C%C, e6)
CALL Display(e6, "5. e6 (engineering):", orient="row")
CALL Display(s6, "5. s6 = C e6:", orient="row")
CALL OK(ABS(e6(4) - 0.02_DFP) .LT. tol .AND. &
        ABS(s6(1) - sig0(1, 1)) .LT. tol .AND. &
        ABS(s6(4) - sig0(1, 2)) .LT. tol, "Voigt 3D")

!----------------------------------------------------------------------------
! 6. 2D plane strain: D (3x3) and [e11, e22, g12] for K = int B^T D B
!----------------------------------------------------------------------------

D = GetTangent(C, VoigtFormPlaneStrain)
e2 = GetVoigtVector(epsV, VoigtFormPlaneStrain)
CALL Display(D, "6. D plane strain:")
CALL Display(MATMUL(D, e2), "6. [s11, s22, s12]:", orient="row")
CALL OK(MAXVAL(ABS(MATMUL(D, e2) - [sig0(1, 1), sig0(2, 2), &
                                    sig0(1, 2)])) .LT. tol, "plane strain")

!----------------------------------------------------------------------------
! 7. Large deformation (St. Venant-Kirchhoff): F = I + grad(u)
!----------------------------------------------------------------------------
!
! E = (F^T F - I)/2, S = C : E (PK2), P = F S for f_int = int P : grad N,
! sigma = F S F^T / J for output

F = DeformationGradient(Eye3 + gradU)
EE = GreenStrain(F)
S = Stress(Contraction(C, EE), StressMeasurePK2)
P = PK1Stress(S, F)
cauchy = CauchyStress(S, F)
CALL Display(EE, "7. Green strain E:")
CALL Display(S, "7. PK2 stress S:")
CALL Display(P, "7. PK1 stress P:")
CALL Display(cauchy, "7. Cauchy stress:")
CALL OK(MAXVAL(ABS(P%T - MATMUL(F%T, S%T))) .LT. tol, "P = F S")
CALL OK(MAXVAL(ABS(cauchy%T - MATMUL(P%T, TRANSPOSE(F%T)) &
                   / Jacobian(F))) .LT. tol, "sigma = P F^T / J")

END PROGRAM main
