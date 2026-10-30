!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: y(:, :)

y = EquidistanceInPoint_Line(order=2_I4B)
CALL OK(ALL(SHAPE(y) == [1, 1]), "Coordinate dimensions")
CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
  "Single interior point is the centroid")
END PROGRAM main
