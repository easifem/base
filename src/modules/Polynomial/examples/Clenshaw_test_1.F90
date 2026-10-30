!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a series using backward Clenshaw recurrence.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE OrthogonalPolynomialUtility, ONLY: Clenshaw
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), a(0:2), b(0:2), c(0:2), y(3)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
a = 0.0_DFP
b = [2.0_DFP, 1.0_DFP/3.0_DFP, 4.0_DFP/15.0_DFP]
c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
y = Clenshaw(x=x, alpha=a, beta=b, c=c)
CALL OK(MAXVAL(ABS(y-(x*x-1.0_DFP/3.0_DFP))) &
  < 1.0E-11_DFP, "Monic Legendre degree two")
END PROGRAM main
