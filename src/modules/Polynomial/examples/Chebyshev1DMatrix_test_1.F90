!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the nodal first derivative matrix.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Gauss
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1DMatrix
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)

CALL Chebyshev1Quadrature(n=3_I4B, pt=x, wt=w, quadType=Gauss)
d = Chebyshev1DMatrix(n=2_I4B, x=x, quadType=Gauss)
CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
  < 1.0E-10_DFP, "Differentiate a nodal quadratic")
END PROGRAM main
