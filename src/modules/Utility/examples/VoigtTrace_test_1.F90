!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtTrace of a 3D Voigt vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtTrace
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: trace is the sum of the normal components
BLOCK
  REAL(DFP) :: v(6)

  v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL OK(ABS(VoigtTrace(v) - 6.0_DFP) .LT. tol, "VoigtTrace: 1 + 2 + 3")
END BLOCK

END PROGRAM main
