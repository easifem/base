!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: EquidistanceInPoint_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: y(:, :)

y = EquidistanceInPoint_Triangle(order=3_I4B)
CALL OK(ALL(SHAPE(y) == [2, 1]), "Coordinate dimensions")
CALL OK(MAXVAL(ABS(y-(1.0_DFP/3.0_DFP))) < 1.0E-11_DFP, &
  "Single interior point is the centroid")
END PROGRAM main
