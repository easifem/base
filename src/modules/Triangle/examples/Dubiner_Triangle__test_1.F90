!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(2,2), y(2,3)
INTEGER(I4B) :: nr,nc

points(:,1) = [0.2_DFP,0.2_DFP]
points(:,2) = [0.1_DFP,0.3_DFP]
CALL Dubiner_Triangle_(order=1_I4B, xij=points, refTriangle="UNIT", &
  ans=y, nrow=nr,ncol=nc)
CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
  "Constant Dubiner mode")
END PROGRAM main
