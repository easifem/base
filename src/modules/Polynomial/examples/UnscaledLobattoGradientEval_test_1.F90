!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate the first derivative.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3)
REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = UnscaledLobattoGradientEval(n=2_I4B, x=x)
CALL OK(MAXVAL(ABS(y-(3.0_DFP*x/3.0_DFP))) < tol, &
  "Degree two agrees with its analytic formula")
CALL OK(ABS(UnscaledLobattoGradientEval(n=2_I4B, x=0.0_DFP) &
  -y(2)) < tol, "Scalar and vector calls agree")
END PROGRAM main
