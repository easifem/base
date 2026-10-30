!> author: OpenAI Codex
! date: 2026-10-30
! summary: Compute Gauss quadrature nodes and weights.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2), w(2)

CALL JacobiGaussQuadrature( &
  alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, pt=x, wt=w)
CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate a constant")
CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate the weighted second moment")
END PROGRAM main
