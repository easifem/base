!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Triangle_Method, ONLY: triangle_edge_length_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: y(3)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
y = triangle_edge_length_2d(t2)
CALL OK(ABS(SUM(y)-(3.0_DFP+SQRT(5.0_DFP))) &
  < 1.0E-11_DFP, "Perimeter from edge lengths")
END PROGRAM main
