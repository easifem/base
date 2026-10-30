!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
INTEGER(I4B) :: i,j,nr,nc

nodes = RefCoord_Tetrahedron("UNIT")
CALL LagrangeCoeff_Tetrahedron_(order=1_I4B,xij=nodes, &
  basisType=poly%Monomial,refTetrahedron="UNIT",ans=c,nrow=nr,ncol=nc)
degree = LagrangeDegree_Tetrahedron(1_I4B)
DO j = 1,4
  DO i = 1,4
    v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
  END DO
END DO
v = MATMUL(v,c)
DO j = 1,4
  v(j,j) = v(j,j)-1.0_DFP
END DO
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
END PROGRAM main
