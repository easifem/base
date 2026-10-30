!> author: OpenAI Codex
! date: 2026-10-30
! summary: Compute Gauss quadrature nodes and weights.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2), w(2)

CALL Chebyshev1GaussQuadrature(n=2_I4B, pt=x, wt=w)
CALL OK(ABS(SUM(w)-(ACOS(-1.0_DFP))) < 1.0E-10_DFP, &
  "Weights integrate a constant")
CALL OK(ABS(SUM(w*x*x)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate the weighted second moment")
END PROGRAM main
