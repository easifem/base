!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: segment_point_near_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
REAL(DFP) :: nearest(2), d, parameter

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
CALL segment_point_near_2d(p1, p2, [3.0_DFP, 1.0_DFP], &
  nearest, d, parameter)
CALL OK(ABS(parameter-1.0_DFP) < 1.0E-12_DFP, "Parameter clamped")
CALL OK(MAXVAL(ABS(nearest-p2)) < 1.0E-12_DFP, &
  "Nearest point is the right endpoint")
CALL OK(ABS(d-SQRT(2.0_DFP)) < 1.0E-12_DFP, "Euclidean distance")
END PROGRAM main
