!> author: OpenAI Codex
! date: 2026-10-30
! summary: Construct equally spaced interpolation points.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: EquidistancePoint_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(1, 3)
INTEGER(I4B) :: nr, nc

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
CALL EquidistancePoint_(order=2_I4B, elemType=Line, xij=nodes(:, [1, 3]), &
  ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(y(1, :))) <= 1.0_DFP .AND. &
  ABS(SUM(y)) < 1.0E-11_DFP, &
  "Interpolation construction")
END PROGRAM main
