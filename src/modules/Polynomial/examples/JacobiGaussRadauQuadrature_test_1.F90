!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute Gauss Radau quadrature nodes and weights.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiGaussRadauQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), w(3)

CALL JacobiGaussRadauQuadrature( &
  alpha=0.0_DFP, beta=0.0_DFP, a=-1.0_DFP, n=2_I4B, pt=x, wt=w)
CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate a constant")
CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate the weighted second moment")
CALL OK(MINVAL(ABS(x+1.0_DFP)) < 1.0E-11_DFP, &
  "Prescribed endpoint is present")
END PROGRAM main
