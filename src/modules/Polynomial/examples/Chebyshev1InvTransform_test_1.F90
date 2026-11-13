!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1InvTransform
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3), c(0:2)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
y = Chebyshev1InvTransform(n=2_I4B, x=x, coeff=c)
CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < 1.0E-11_DFP, &
  "A single modal coefficient recovers degree two")
END PROGRAM main
