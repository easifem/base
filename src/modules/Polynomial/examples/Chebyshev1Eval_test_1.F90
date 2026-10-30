!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate one polynomial.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Eval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3)
REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = Chebyshev1Eval(n=2_I4B, x=x)
CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < tol, &
  "Degree two agrees with its analytic formula")
CALL OK(ABS(Chebyshev1Eval(n=2_I4B, x=0.0_DFP) &
  -y(2)) < tol, "Scalar and vector calls agree")
END PROGRAM main
