!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtStressNorm is the Frobenius norm of the tensor

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtStressNorm
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-13_DFP

! Test 1: same as SQRT(SUM(T * T)) of the 3 x 3 tensor
BLOCK
  REAL(DFP) :: v(6), T(3, 3)

  v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  T = RESHAPE([v(1), v(4), v(6), &
               v(4), v(2), v(5), &
               v(6), v(5), v(3)], [3, 3])
  CALL OK(ABS(VoigtStressNorm(v) - SQRT(SUM(T * T))) .LT. tol, &
          "VoigtStressNorm: Frobenius norm")
  CALL OK(ABS(VoigtStressNorm(v) - SQRT(168.0_DFP)) .LT. tol, &
          "VoigtStressNorm: sqrt(168)")
END BLOCK

END PROGRAM main
