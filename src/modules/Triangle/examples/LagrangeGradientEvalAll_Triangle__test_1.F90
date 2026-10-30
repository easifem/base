!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE TriangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,3), points(2,3)
REAL(DFP) :: y(3,3,2)
INTEGER(I4B) :: nr,nc,nd

nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
  0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
points = nodes
CALL LagrangeGradientEvalAll_Triangle_(order=1_I4B, x=points, &
  xij=nodes, basisType=poly%Monomial, refTriangle="UNIT", ans=y, &
  dim1=nr, dim2=nc, dim3=nd)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
  "Gradient of partition of unity")
END PROGRAM main
