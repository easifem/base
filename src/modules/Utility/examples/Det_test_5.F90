!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: DET of matrices larger than 5 x 5
!
! Description:
!   Regression test for bug M-19 in bug-reports/Utility_bug_report.md:
!   DET had no formula for sizes above 5, so the result was undefined.
!   Larger matrices now use Gaussian elimination with partial pivoting.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: DET
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a(6, 6), b(7, 7), c(6, 6)
INTEGER(I4B) :: i

! tridiagonal matrix with 2 on the diagonal and -1 next to it,
! det = n + 1
a = 0.0_DFP
DO i = 1, 6
  a(i, i) = 2.0_DFP
  IF (i .GT. 1) a(i, i - 1) = -1.0_DFP
  IF (i .LT. 6) a(i, i + 1) = -1.0_DFP
END DO
CALL OK(ABS(DET(a) - 7.0_DFP) .LT. tol, "6 x 6 tridiagonal, det = 7")

! swapping two rows changes the sign
c = a
c([1, 2], :) = a([2, 1], :)
CALL OK(ABS(DET(c) + 7.0_DFP) .LT. tol, "row swap changes the sign")

! diagonal 7 x 7 matrix, det = 7!
b = 0.0_DFP
DO i = 1, 7
  b(i, i) = REAL(i, DFP)
END DO
CALL OK(ABS(DET(b) - 5040.0_DFP) .LT. tol, "7 x 7 diagonal, det = 7!")

! a singular 6 x 6 matrix: two equal rows
c = a
c(3, :) = c(2, :)
CALL OK(ABS(DET(c)) .LT. tol, "singular 6 x 6")

END PROGRAM main
