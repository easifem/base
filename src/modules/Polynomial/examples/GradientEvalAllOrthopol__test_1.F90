!> author: OpenAI Codex
! date: 2026-10-30
! summary: Dispatch basis gradients by polynomial family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP) :: y(3, 3)
INTEGER(I4B) :: nr, nc

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
CALL GradientEvalAllOrthopol_(n=2_I4B, x=x, orthopol=poly%Legendre, &
  ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
END PROGRAM main
