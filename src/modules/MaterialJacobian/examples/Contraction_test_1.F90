!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Contraction of an isotropic tangent with a strain

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lam = 2.0_DFP
REAL(DFP), PARAMETER :: mu = 3.0_DFP
REAL(DFP), PARAMETER :: e0(3, 3) = RESHAPE( &
                        [1.0_DFP, 0.2_DFP, -0.3_DFP, &
                         0.2_DFP, 2.0_DFP, 0.4_DFP, &
                         -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
TYPE(MaterialJacobian_) :: obj
REAL(DFP) :: expected(3, 3)

obj = MaterialJacobian(IsotropicTangent(lam, mu))
expected = lam * (e0(1, 1) + e0(2, 2) + e0(3, 3)) * Eye3 &
           + 2.0_DFP * mu * e0

! Test 1: strain as a Rank2Tensor_
BLOCK
  TYPE(Rank2Tensor_) :: sig

  sig = Contraction(obj, Rank2Tensor(e0, .TRUE.))
  CALL OK(MAXVAL(ABS(sig%T - expected)) .LT. tol, "Rank2Tensor: stress")
  CALL OK(sig%isSym, "Rank2Tensor: stress is symmetric")
END BLOCK

! Test 2: strain as a strain-like or stress-like VoigtRank2Tensor_
BLOCK
  TYPE(VoigtRank2Tensor_) :: vsig
  REAL(DFP) :: T(3, 3)

  vsig = Contraction(obj, VoigtRank2Tensor(e0, StrainTypeVoigt))
  T = vsig
  CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "Voigt strain-like")
  vsig = Contraction(obj, VoigtRank2Tensor(e0, StressTypeVoigt))
  T = vsig
  CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "Voigt stress-like")
  CALL OK(vsig%voigtType .EQ. StressTypeVoigt, "Voigt: result type")
END BLOCK

END PROGRAM main
