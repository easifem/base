!> author: Shion Shimizu
! date: 2026-11-17
! summary: Otimes of two VoigtRank2Tensor_

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Otimes
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtUtility, ONLY: VoigtIdentity
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: I otimes I
BLOCK
  TYPE(VoigtRank2Tensor_) :: eye

  eye = VoigtRank2Tensor(VoigtIdentity(), StressTypeVoigt)
  CALL OK(MAXVAL(ABS(Otimes(eye, eye) - Rank4IdentityOtimesIdentity())) &
          .LT. tol, "Otimes: I otimes I")
END BLOCK

! Test 2: (a otimes b) : eps = a (b : eps) for an engineering strain eps,
! also when b is given as an engineering strain
BLOCK
  TYPE(VoigtRank2Tensor_) :: a, b, bEng
  REAL(DFP) :: eps(6), bDotEps

  a = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                        6.0_DFP], StressTypeVoigt)
  b = VoigtRank2Tensor([6.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, &
                        1.0_DFP], StressTypeVoigt)
  bEng = VoigtRank2Tensor([6.0_DFP, 5.0_DFP, 4.0_DFP, 6.0_DFP, 4.0_DFP, &
                           2.0_DFP], StrainTypeVoigt)
  eps = [0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP, 0.5_DFP, 0.6_DFP]
  bDotEps = DOT_PRODUCT(b%V, eps)
  CALL OK(MAXVAL(ABS(MATMUL(Otimes(a, b), eps) - a%V * bDotEps)) &
          .LT. tol, "Otimes: (a otimes b) : eps")
  CALL OK(MAXVAL(ABS(Otimes(a, bEng) - Otimes(a, b))) .LT. tol, &
          "Otimes: b as an engineering strain")
END BLOCK

END PROGRAM main
