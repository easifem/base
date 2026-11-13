!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
INTEGER(I4B) :: i, j, nr, nc

nodes(1,:) = [-1.0_DFP,1.0_DFP]
CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
  basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
degree = LagrangeDegree_Line(1_I4B)
DO j = 1, 2
  DO i = 1, 2
    v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
  END DO
END DO
v = MATMUL(v,c)
DO j = 1, 2
  v(j,j) = v(j,j)-1.0_DFP
END DO
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
END PROGRAM main
