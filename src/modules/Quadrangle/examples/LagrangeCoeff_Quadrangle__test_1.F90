!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
INTEGER(I4B) :: i,j,nr,nc

nodes = RefCoord_Quadrangle("BIUNIT")
CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
  basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
degree = LagrangeDegree_Quadrangle(1_I4B)
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
