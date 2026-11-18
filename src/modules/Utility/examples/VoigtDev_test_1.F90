!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtDev of a stress-like 3D Voigt vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtDev
USE VoigtUtility, ONLY: VoigtTrace
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: mean of the normal components is removed, shear is unchanged
BLOCK
  REAL(DFP) :: v(6), s(6), expected(6)

  v = [1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
  expected = [-2.0_DFP, -1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
  s = VoigtDev(v)
  CALL OK(MAXVAL(ABS(s - expected)) .LT. tol, "VoigtDev: values")
  CALL OK(ABS(VoigtTrace(s)) .LT. tol, "VoigtDev: trace is zero")
END BLOCK

END PROGRAM main
