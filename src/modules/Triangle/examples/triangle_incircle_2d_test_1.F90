!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for Triangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Triangle_Method, ONLY: triangle_incircle_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
REAL(DFP) :: y, center(2)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
CALL triangle_incircle_2d(t2, y, center)
CALL OK(ABS(y-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) < 1.0E-11_DFP, "Inradius")
CALL OK(MAXVAL(ABS(center-y)) < 1.0E-11_DFP, "Incenter")
END PROGRAM main
