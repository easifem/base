!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: INV of matrices larger than 4 x 4
!
! Description:
!   Regression test for bug M-19 in bug-reports/Utility_bug_report.md:
!   INV left invA unchanged for sizes above 4. Larger matrices now use
!   Gauss-Jordan elimination with partial pivoting.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: INV
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a(6, 6), inva(6, 6), eye(6, 6), p(5, 5), invp(5, 5)
REAL(DFP) :: s(6, 6), invs(6, 6)
INTEGER(I4B) :: i

eye = 0.0_DFP
DO i = 1, 6
  eye(i, i) = 1.0_DFP
END DO

! tridiagonal 6 x 6 matrix
a = 0.0_DFP
DO i = 1, 6
  a(i, i) = 2.0_DFP
  IF (i .GT. 1) a(i, i - 1) = -1.0_DFP
  IF (i .LT. 6) a(i, i + 1) = -1.0_DFP
END DO
CALL INV(invA=inva, A=a)
CALL OK(ALL(ABS(MATMUL(a, inva) - eye) .LT. tol), "6 x 6: A inv(A) = I")
CALL OK(ALL(ABS(MATMUL(inva, a) - eye) .LT. tol), "6 x 6: inv(A) A = I")
! known entry of the inverse: (inv A)(1, 1) = 6 / 7
CALL OK(ABS(inva(1, 1) - 6.0_DFP / 7.0_DFP) .LT. tol, "6 x 6: entry")

! 5 x 5 matrix that needs pivoting (zero on the first diagonal entry)
p = 0.0_DFP
DO i = 1, 5
  p(i, MOD(i, 5) + 1) = REAL(i, DFP)
END DO
CALL INV(invA=invp, A=p)
CALL OK(ALL(ABS(MATMUL(p, invp) - eye(1:5, 1:5)) .LT. tol), &
        "5 x 5 with pivoting: A inv(A) = I")

! a singular 6 x 6 matrix gives zero
s = a
s(4, :) = s(3, :)
invs = 1.0_DFP
CALL INV(invA=invs, A=s)
CALL OK(ALL(invs .EQ. 0.0_DFP), "singular 6 x 6 gives zero")

END PROGRAM main
