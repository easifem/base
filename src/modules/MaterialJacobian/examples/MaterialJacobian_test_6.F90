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


!> Test: plane strain J2 elastoplasticity at one quadrature point
! summary: 3D return mapping, 4-component history, 2D residual/stiffness
!
! In plane strain eps_zz = 0, but sigma_zz is not zero, and the yield
! function needs the full deviatoric stress. So:
!
! - The history at each quadrature point is stored with 4 components in
!   the order [11, 22, 12, 33] (`VoigtFormAxisymmetric` ordering):
!   sig4 (stress) and epsP4 (plastic strain, engineering shear).
! - The constitutive update works in 3D with Rank2Tensor_ (Dev, norms).
! - The residual and stiffness only use the in-plane part:
!   sig4(1:3) = [s11, s22, s12] and Cep([1,2,4], [1,2,4]).
!
! Model: von Mises with linear isotropic hardening, radial return and
! consistent tangent (Simo and Hughes, Computational Inelasticity, 1998,
! Box 3.2):
!
!   f = ||s|| - sqrt(2/3) (sigY + H alpha)
!   dgamma = f_trial / (2 mu + 2 H / 3)
!   Cep = K I x I + 2 mu theta Pdev - 2 mu thetaBar n x n

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: MaterialJacobian_
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE Display_Method, ONLY: Display
IMPLICIT NONE

! Material
REAL(DFP), PARAMETER :: lam = 2.0_DFP
REAL(DFP), PARAMETER :: mu = 1.0_DFP
REAL(DFP), PARAMETER :: H = 0.1_DFP
REAL(DFP), PARAMETER :: sigY = 0.03_DFP

! History variables of one quadrature point: 4 + 4 + 1 reals
REAL(DFP) :: sig4(4), epsP4(4), alpha

! What the element routine needs
REAL(DFP) :: s3(3), D3(3, 3), gradU2(2, 2)
TYPE(MaterialJacobian_) :: Ce
TYPE(Rank2Tensor_) :: sig
INTEGER(I4B) :: istep

! The elastic tangent is built once
Ce = MaterialJacobian(IsotropicTangent(lam, mu))

sig4 = 0.0_DFP
epsP4 = 0.0_DFP
alpha = 0.0_DFP

DO istep = 1, 3
  ! In a FE code: gradU2 = MATMUL(uNodes(1:2, :), dNdX(:, 1:2))
  gradU2 = 0.0_DFP
  gradU2(1, 1) = 0.01_DFP * istep
  gradU2(1, 2) = 0.02_DFP * istep

  CALL ReturnMap(Ce, gradU2, epsP4, alpha, sig4, s3, D3)

  ! sig4 holds the full stress, including sigma_zz
  sig = Rank2Tensor(Stress4ToMat(sig4), .TRUE.)

  WRITE (*, "(A, I2)") "step ", istep
  CALL Display(sig4, "sig4 [s11, s22, s12, szz]:", orient="row")
  WRITE (*, "(A, ES12.5)") "von Mises = ", EquivalentStress(sig)
  WRITE (*, "(A, ES12.5)") "alpha     = ", alpha

  ! Element level (not shown): f_int += B^T s3 w, K += B^T D3 B w
  CALL Display(s3, "s3 for residual:", orient="row")
  CALL Display(D3, "D3 for stiffness:")
END DO

CONTAINS

!----------------------------------------------------------------------------
!                                                                  ReturnMap
!----------------------------------------------------------------------------

! Plane strain radial return. Input: elastic tangent, in-plane gradient
! of u, and history (updated in place). Output: 4-component stress and
! the in-plane stress and consistent tangent for the element routine.

SUBROUTINE ReturnMap(Ce, gradU2, epsP4, alpha, sig4, s3, D3)
  USE GlobalData, ONLY: StrainTypeVoigt
  USE GlobalData, ONLY: StressTypeVoigt
  USE GlobalData, ONLY: VoigtFormAxisymmetric
  USE GlobalData, ONLY: VoigtFormPlaneStrain
  USE BaseType, ONLY: VoigtRank2Tensor_
  USE Rank2Tensor_Method, ONLY: SmallStrain
  USE Rank2Tensor_Method, ONLY: Dev
  USE Rank2Tensor_Method, ONLY: OPERATOR(-)
  USE Rank2Tensor_Method, ONLY: OPERATOR(*)
  USE Rank2Tensor_Method, ONLY: OPERATOR(+)
  USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
  USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
  USE MaterialJacobian_Method, ONLY: Contraction
  USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
  USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
  USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector

  TYPE(MaterialJacobian_), INTENT(IN) :: Ce
  REAL(DFP), INTENT(IN) :: gradU2(2, 2)
  REAL(DFP), INTENT(INOUT) :: epsP4(4)
  REAL(DFP), INTENT(INOUT) :: alpha
  REAL(DFP), INTENT(OUT) :: sig4(4)
  REAL(DFP), INTENT(OUT) :: s3(3)
  REAL(DFP), INTENT(OUT) :: D3(3, 3)

  REAL(DFP), PARAMETER :: sqrt23 = SQRT(2.0_DFP / 3.0_DFP)
  REAL(DFP) :: gradU(3, 3), Cep(6, 6), n6(6), normS, f, dgamma
  REAL(DFP) :: theta, thetaBar, bulk
  TYPE(Rank2Tensor_) :: eps, epsP, sigTr, s, n, sig
  TYPE(VoigtRank2Tensor_) :: vtmp

  ! 1. 3D strain from the 2D gradient: eps_zz = eps_xz = eps_yz = 0
  gradU = 0.0_DFP
  gradU(1:2, 1:2) = gradU2
  eps = SmallStrain(gradU)

  ! 2. Plastic strain from the 4-component history (eps^p_zz included)
  vtmp = VoigtRank2Tensor(epsP4, StrainTypeVoigt, VoigtFormAxisymmetric)
  epsP = Rank2Tensor(vtmp)

  ! 3. Elastic trial stress in 3D (sigma_zz /= 0 comes out naturally)
  sigTr = Contraction(Ce, eps - epsP)
  s = Dev(sigTr)
  normS = SQRT(SUM(s%T**2))
  f = normS - sqrt23 * (sigY + H * alpha)

  IF (f .LE. 0.0_DFP) THEN
    ! Elastic step
    sig = sigTr
    Cep = Ce%C
  ELSE
    ! Plastic step: radial return
    dgamma = f / (2.0_DFP * mu + 2.0_DFP * H / 3.0_DFP)
    n = (1.0_DFP / normS) * s
    sig = sigTr - (2.0_DFP * mu * dgamma) * n
    epsP = epsP + dgamma * n
    alpha = alpha + sqrt23 * dgamma

    ! Consistent tangent in 6x6 Voigt (C(I,J) = C_ijkl convention, so
    ! n x n uses the tensor components of n, not engineering shear)
    vtmp = VoigtRank2Tensor(n%T, StressTypeVoigt)
    n6 = vtmp%V
    theta = 1.0_DFP - 2.0_DFP * mu * dgamma / normS
    thetaBar = 1.0_DFP / (1.0_DFP + H / (3.0_DFP * mu)) - (1.0_DFP - theta)
    bulk = lam + 2.0_DFP * mu / 3.0_DFP
    Cep = bulk * Rank4IdentityOtimesIdentity() &
          + 2.0_DFP * mu * theta * Rank4DeviatoricProjector() &
          - 2.0_DFP * mu * thetaBar * SPREAD(n6, 2, 6) * SPREAD(n6, 1, 6)
  END IF

  ! 4. Store the history with 4 components [11, 22, 12, 33]
  vtmp = VoigtRank2Tensor(sig%T, StressTypeVoigt)
  sig4 = GetVoigtVector(vtmp, VoigtFormAxisymmetric)
  vtmp = VoigtRank2Tensor(epsP%T, StrainTypeVoigt)
  epsP4 = GetVoigtVector(vtmp, VoigtFormAxisymmetric)

  ! 5. What the element needs: in-plane stress and 3x3 tangent.
  !    Plane strain needs no condensation because eps_zz = 0.
  s3 = sig4(1:3)
  D3 = ReduceVoigtTangent(Cep, VoigtFormPlaneStrain)
END SUBROUTINE ReturnMap

!----------------------------------------------------------------------------
!                                                               Stress4ToMat
!----------------------------------------------------------------------------

! 3x3 stress from [s11, s22, s12, szz]

PURE FUNCTION Stress4ToMat(v4) RESULT(T)
  REAL(DFP), INTENT(IN) :: v4(4)
  REAL(DFP) :: T(3, 3)

  T = 0.0_DFP
  T(1, 1) = v4(1)
  T(2, 2) = v4(2)
  T(1, 2) = v4(3)
  T(2, 1) = v4(3)
  T(3, 3) = v4(4)
END FUNCTION Stress4ToMat

END PROGRAM main
