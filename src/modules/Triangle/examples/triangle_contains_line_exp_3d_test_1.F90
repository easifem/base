!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Triangle_Method, ONLY: triangle_contains_line_exp_3d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: p(3)
LOGICAL(LGT) :: inside

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
CALL triangle_contains_line_exp_3d(t3, &
  [0.5_DFP, 0.25_DFP, -1.0_DFP], [0.5_DFP, 0.25_DFP, 1.0_DFP], inside, p)
CALL OK(inside, "Line crosses triangle interior")
CALL OK(ABS(p(3)) < 1.0E-11_DFP, "Intersection lies in plane")
END PROGRAM main
