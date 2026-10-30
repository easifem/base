!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Triangle_Method, ONLY: triangle_sample
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: y(2, 8)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
y = triangle_sample(t2, n=8_I4B, seed=12345_I4B)
CALL OK(MINVAL(y) >= 0.0_DFP, "Samples have nonnegative coordinates")
CALL OK(MAXVAL(y(1, :)/2.0_DFP+y(2, :)) <= 1.0_DFP, &
  "Samples lie inside the right triangle")
END PROGRAM main
