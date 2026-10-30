!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
REAL(DFP), PARAMETER :: h = 1.0E-5_DFP

! The declared Laplacian routine has no implementation.
! Approximate the second derivative using the working gradient API.
x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
d2 = (gp-gm)/(2.0_DFP*h)
CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
  "Gradient based second derivative workaround")
END PROGRAM main
