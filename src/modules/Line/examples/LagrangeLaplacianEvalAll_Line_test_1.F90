!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE LineInterpolationUtility, ONLY: LagrangeLaplacianEvalAll_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1,3), points(1,3), y(3,3)

nodes(1,:) = [-1.0_DFP,0.0_DFP,1.0_DFP]
points = nodes
y = LagrangeLaplacianEvalAll_Line(order=2_I4B,x=points,xij=nodes, &
  basisType=poly%Monomial)
CALL OK(MAXVAL(ABS(y(:,2)+2.0_DFP)) < 1.0E-10_DFP, &
  "Central cardinal quadratic has second derivative -2")
END PROGRAM main
