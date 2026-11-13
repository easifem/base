!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Gauss
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform_
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), w(3), f(3), c(0:2)
INTEGER(I4B) :: ns

CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
f = 2.0_DFP*x*x-1.0_DFP
CALL Chebyshev1Transform_( &
  n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
CALL OK(MAXVAL(ABS(c- &
  [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
  "Recover the single degree two modal coefficient")
END PROGRAM main
