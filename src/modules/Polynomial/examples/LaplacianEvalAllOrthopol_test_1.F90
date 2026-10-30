!> author: OpenAI Codex
! date: 2026-10-30
! summary: Dispatch basis Laplacians by polynomial family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE OrthogonalPolynomialUtility, ONLY: LaplacianEvalAllOrthopol
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP), ALLOCATABLE :: y(:, :)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = LaplacianEvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Monomial)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
END PROGRAM main
