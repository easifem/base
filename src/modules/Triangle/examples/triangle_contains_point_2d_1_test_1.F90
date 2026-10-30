!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Triangle_Method, ONLY: triangle_contains_point_2d_1
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
LOGICAL(LGT) :: inside

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
inside = triangle_contains_point_2d_1(t2, [0.5_DFP, 0.25_DFP])
CALL OK(inside, "Interior point is contained")
CALL OK(.NOT. triangle_contains_point_2d_1(t2, [3.0_DFP, 2.0_DFP]), &
  "Exterior point is rejected")
END PROGRAM main
