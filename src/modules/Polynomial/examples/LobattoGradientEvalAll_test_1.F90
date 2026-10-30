!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3, 3)
REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = LobattoGradientEvalAll(n=2_I4B, x=x)
CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
  "Third column contains degree two")
END PROGRAM main
