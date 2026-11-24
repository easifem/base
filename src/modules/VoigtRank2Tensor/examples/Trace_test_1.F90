!> author: Shion Shimizu
! date: 2026-11-17
! summary: Trace of a VoigtRank2Tensor_

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Trace
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                5.0_DFP, 6.0_DFP]

! Test 1: stress-like and strain-like tensors have the same trace
BLOCK
  TYPE(VoigtRank2Tensor_) :: sig, eps

  sig = VoigtRank2Tensor(v, StressTypeVoigt)
  eps = VoigtRank2Tensor(v, StrainTypeVoigt)
  CALL OK(ABS(Trace(sig) - 6.0_DFP) .LT. tol, "Trace: stress")
  CALL OK(ABS(Trace(eps) - 6.0_DFP) .LT. tol, "Trace: strain")
END BLOCK

END PROGRAM main
