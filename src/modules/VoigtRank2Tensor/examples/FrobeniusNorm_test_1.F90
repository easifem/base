!> author: Shion Shimizu
! date: 2026-11-17
! summary: FrobeniusNorm of a VoigtRank2Tensor_

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: FrobeniusNorm
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-13_DFP

! Test 1: stress-like tensor, shear entries are tensor components
BLOCK
  TYPE(VoigtRank2Tensor_) :: sig

  sig = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                          6.0_DFP], StressTypeVoigt)
  CALL OK(ABS(FrobeniusNorm(sig) - SQRT(168.0_DFP)) .LT. tol, &
          "FrobeniusNorm: stress")
END BLOCK

! Test 2: the same tensor as an engineering strain gives the same norm
BLOCK
  TYPE(VoigtRank2Tensor_) :: eps

  eps = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, 10.0_DFP, &
                          12.0_DFP], StrainTypeVoigt)
  CALL OK(ABS(FrobeniusNorm(eps) - SQRT(168.0_DFP)) .LT. tol, &
          "FrobeniusNorm: engineering strain")
END BLOCK

END PROGRAM main
