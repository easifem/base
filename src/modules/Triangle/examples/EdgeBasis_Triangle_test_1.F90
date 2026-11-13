!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: EdgeBasis_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(2,3), y(3,3)

points = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
  0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
y = EdgeBasis_Triangle(pe1=2_I4B,pe2=2_I4B,pe3=2_I4B, xij=points, &
  refTriangle="UNIT")
CALL OK(MAXVAL(ABS(y)) < 1.0E-11_DFP, &
  "Interior entity modes vanish at the vertices")
END PROGRAM main
