!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate one polynomial.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3)
REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
  "Degree two agrees with its analytic formula")
CALL OK(ABS(JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
  -y(2)) < tol, "Scalar and vector calls agree")
END PROGRAM main
