!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians on a reference element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE GlobalData, ONLY: Line
USE OrthogonalPolynomialUtility, ONLY: OrthogonalLaplacianEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP) :: points(1, 3)
REAL(DFP) :: y(3, 3)
INTEGER(I4B) :: nr, nc

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
points(1, :) = x
CALL OrthogonalLaplacianEvalAll_(order=2_I4B, elemType=Line, xij=points, &
  domainName="BIUNIT", basisType=poly%Monomial, &
  ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
END PROGRAM main
