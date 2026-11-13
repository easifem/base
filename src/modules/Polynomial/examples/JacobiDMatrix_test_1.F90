!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the nodal first derivative matrix.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Gauss
USE JacobiPolynomialUtility, ONLY: JacobiDMatrix
USE JacobiPolynomialUtility, ONLY: JacobiQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)

CALL JacobiQuadrature( &
  alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w, quadType=Gauss)
d = JacobiDMatrix( &
  alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, quadType=Gauss)
CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
  < 1.0E-10_DFP, "Differentiate a nodal quadratic")
END PROGRAM main
