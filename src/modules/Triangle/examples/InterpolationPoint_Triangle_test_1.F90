!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: y(:, :)

y = InterpolationPoint_Triangle(order=2_I4B, ipType=Equidistance, &
  layout="VEFC")
CALL OK(ALL(SHAPE(y) == [2, 6]), "Coordinate dimensions")
CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
END PROGRAM main
