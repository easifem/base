!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: y(:, :)

y = InterpolationPoint_Line(order=2_I4B, ipType=Equidistance, layout="VEFC")
CALL OK(ALL(SHAPE(y) == [1, 3]), "Coordinate dimensions")
CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
END PROGRAM main
