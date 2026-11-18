!> author: Shion Shimizu
! date: 2026-11-17
! summary: Eigen of a VoigtRank2Tensor_

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Eigen
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: diagonal stress, eigenvalues in descending order
BLOCK
  TYPE(VoigtRank2Tensor_) :: sig
  REAL(DFP) :: QR(3, 3), WR(3)

  sig = VoigtRank2Tensor([1.0_DFP, 3.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, &
                          0.0_DFP], StressTypeVoigt)
  CALL Eigen(obj=sig, QR=QR, WR=WR)
  CALL OK(MAXVAL(ABS(WR - [3.0_DFP, 2.0_DFP, 1.0_DFP])) .LT. tol, &
          "Eigen: values")
  CALL OK(ABS(ABS(QR(2, 1)) - 1.0_DFP) .LT. tol, &
          "Eigen: first vector is e2")
END BLOCK

! Test 2: an engineering strain has the eigenvalues of the strain tensor
BLOCK
  TYPE(VoigtRank2Tensor_) :: sig, eps
  REAL(DFP) :: QR1(3, 3), WR1(3), QR2(3, 3), WR2(3)
  INTEGER(I4B) :: ii

  sig = VoigtRank2Tensor([4.0_DFP, 1.0_DFP, 2.0_DFP, 0.5_DFP, 0.3_DFP, &
                          0.2_DFP], StressTypeVoigt)
  eps = VoigtRank2Tensor([4.0_DFP, 1.0_DFP, 2.0_DFP, 1.0_DFP, 0.6_DFP, &
                          0.4_DFP], StrainTypeVoigt)
  CALL Eigen(obj=sig, QR=QR1, WR=WR1)
  CALL Eigen(obj=eps, QR=QR2, WR=WR2)
  CALL OK(MAXVAL(ABS(WR1 - WR2)) .LT. tol, "Eigen: strain, values")
  DO ii = 1, 3
    CALL OK(ABS(ABS(DOT_PRODUCT(QR1(:, ii), QR2(:, ii))) - 1.0_DFP) &
            .LT. tol, "Eigen: strain, vectors")
  END DO
END BLOCK

END PROGRAM main
