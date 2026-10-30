!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a Chebyshev series with a halved constant term.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE OrthogonalPolynomialUtility, ONLY: ChebClenshaw
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), c(0:2), y(3)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
c = [2.0_DFP, 0.0_DFP, 1.0_DFP]
y = ChebClenshaw(x=x, c=c)
CALL OK(MAXVAL(ABS(y-2.0_DFP*x*x)) < 1.0E-11_DFP, &
  "Chebyshev series uses half the constant coefficient")
END PROGRAM main
