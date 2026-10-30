!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate the derivative of a polynomial expansion.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalSum
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3), c(0:2)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
y = Chebyshev1GradientEvalSum(n=2_I4B, x=x, coeff=c)
CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < 1.0E-11_DFP, &
  "A single modal coefficient recovers degree two")
END PROGRAM main
