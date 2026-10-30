!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3, 3)
REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
INTEGER(I4B) :: nr, nc

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
CALL Chebyshev1GradientEvalAll_( &
  n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
CALL OK(MAXVAL(ABS(y(:, 3)-(4.0_DFP*x))) < tol, &
  "Third column contains degree two")
END PROGRAM main
