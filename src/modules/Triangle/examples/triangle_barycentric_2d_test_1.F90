!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Triangle_Method, ONLY: triangle_barycentric_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: y(3)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
y = triangle_barycentric_2d(t2, [0.5_DFP, 0.25_DFP])
CALL OK(ABS(SUM(y)-1.0_DFP) < 1.0E-11_DFP, "Barycentric sum")
CALL OK(MAXVAL(ABS(MATMUL(t2, y)- &
  [0.5_DFP, 0.25_DFP])) < 1.0E-11_DFP, "Reconstruct point")
END PROGRAM main
