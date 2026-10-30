!> author: OpenAI Codex
! date: 2026-10-30
! summary: Build the monomial Vandermonde matrix for an element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(3, 3)

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
y = LagrangeVandermonde(order=2_I4B, elemType=Line, xij=nodes)
CALL OK(MAXVAL(ABS(y(:, 3)-nodes(1, :)**2)) < 1.0E-11_DFP, &
  "Interpolation construction")
END PROGRAM main
