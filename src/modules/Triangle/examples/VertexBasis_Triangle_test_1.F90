!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: VertexBasis_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(2,3), y(3,3)

points = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
  0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
y = VertexBasis_Triangle(xij=points, refTriangle="UNIT")
CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
  "Barycentric vertex partition")
END PROGRAM main
