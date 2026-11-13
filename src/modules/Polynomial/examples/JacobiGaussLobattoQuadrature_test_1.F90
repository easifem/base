!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute Gauss Lobatto quadrature nodes and weights.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiGaussLobattoQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(4), w(4)

CALL JacobiGaussLobattoQuadrature( &
  alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, pt=x, wt=w)
CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate a constant")
CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
  "Weights integrate the weighted second moment")
CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
  ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
END PROGRAM main
