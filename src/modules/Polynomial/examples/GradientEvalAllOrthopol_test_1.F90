!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis gradients by polynomial family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP), ALLOCATABLE :: y(:, :)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = GradientEvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Legendre)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
END PROGRAM main
