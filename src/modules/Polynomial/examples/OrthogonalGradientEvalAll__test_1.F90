!> author: OpenAI Codex
! date: 2026-10-30
! summary: Dispatch basis gradients on a reference element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE GlobalData, ONLY: Line
USE OrthogonalPolynomialUtility, ONLY: OrthogonalGradientEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP) :: points(1, 3)
REAL(DFP) :: y(3, 3, 1)
INTEGER(I4B) :: nr, nc, nd

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
points(1, :) = x
CALL OrthogonalGradientEvalAll_(order=2_I4B, elemType=Line, xij=points, &
  domainName="BIUNIT", basisType=poly%Legendre, &
  ans=y, dim1=nr, dim2=nc, dim3=nd)
CALL OK(MAXVAL(ABS(y(:, 3, 1)- &
  (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
END PROGRAM main
