!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtOuter product of two 3D Voigt vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtIdentity
USE VoigtUtility, ONLY: VoigtOuter
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: I otimes I
BLOCK
  CALL OK(MAXVAL(ABS(VoigtOuter(VoigtIdentity(), VoigtIdentity()) &
                     - Rank4IdentityOtimesIdentity())) .LT. tol, &
          "VoigtOuter: I otimes I")
END BLOCK

! Test 2: ans(i, j) = a(i) b(j)
BLOCK
  REAL(DFP) :: a(6), b(6), C(6, 6)

  a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  b = [6.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
  C = VoigtOuter(a, b)
  CALL OK(ABS(C(2, 5) - 4.0_DFP) .LT. tol, "VoigtOuter: C(2, 5)")
  CALL OK(ABS(C(6, 1) - 36.0_DFP) .LT. tol, "VoigtOuter: C(6, 1)")
  CALL OK(MAXVAL(ABS(MATMUL(C, a) - a * DOT_PRODUCT(b, a))) .LT. tol, &
          "VoigtOuter: (a b^T) x = a (b . x)")
END BLOCK

END PROGRAM main
