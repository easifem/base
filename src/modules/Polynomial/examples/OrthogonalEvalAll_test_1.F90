!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis values on a reference element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE GlobalData, ONLY: Line
USE OrthogonalPolynomialUtility, ONLY: OrthogonalEvalAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP) :: points(1, 3)
REAL(DFP), ALLOCATABLE :: y(:, :)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
points(1, :) = x
y = OrthogonalEvalAll(order=2_I4B, elemType=Line, xij=points, &
  domainName="BIUNIT", basisType=poly%Legendre)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
END PROGRAM main
