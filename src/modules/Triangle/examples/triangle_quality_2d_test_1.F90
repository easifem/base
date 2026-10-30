!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Triangle_Method, ONLY: triangle_quality_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: y

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
y = triangle_quality_2d(t2)
CALL OK(y > 0.0_DFP .AND. y <= 1.0_DFP, &
  "Nondegenerate triangle quality is in (0,1]")
END PROGRAM main
