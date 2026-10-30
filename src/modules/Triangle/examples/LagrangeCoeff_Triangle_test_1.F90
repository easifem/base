!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
INTEGER(I4B) :: i, j, nr, nc

nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
  0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
c = LagrangeCoeff_Triangle(order=1_I4B, xij=nodes, &
  basisType=poly%Monomial, refTriangle="UNIT")
degree = LagrangeDegree_Triangle(1_I4B)
DO j = 1, 3
  DO i = 1, 3
    v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
  END DO
END DO
v = MATMUL(v,c)
DO j = 1, 3
  v(j,j) = v(j,j)-1.0_DFP
END DO
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
END PROGRAM main
