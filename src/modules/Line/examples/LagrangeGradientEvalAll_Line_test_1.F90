!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1,2), points(1,2)
REAL(DFP) :: y(2,2,1)

nodes(1,:) = [-1.0_DFP,1.0_DFP]
points = nodes
y = LagrangeGradientEvalAll_Line(order=1_I4B, x=points, xij=nodes, &
  basisType=poly%Monomial)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
  "Gradient of partition of unity")
END PROGRAM main
