!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(2,2), y(2,3,2)
INTEGER(I4B) :: nr,nc,nd

points(:,1) = [0.2_DFP,0.2_DFP]
points(:,2) = [0.1_DFP,0.3_DFP]
CALL HeirarchicalBasisGradient_Triangle_(order=1_I4B, xij=points, &
  refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, &
  tsize1=nr,tsize2=nc,tsize3=nd)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
  "Vertex partition identity")
END PROGRAM main
