!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1,2), points(1,2)
REAL(DFP) :: y(2,2)
INTEGER(I4B) :: nr,nc

nodes(1,:) = [-1.0_DFP,1.0_DFP]
points = nodes
CALL LagrangeEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
  basisType=poly%Monomial, ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
  "Partition of unity")
CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
  ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
END PROGRAM main
