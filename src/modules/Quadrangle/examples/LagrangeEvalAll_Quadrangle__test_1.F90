!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle_
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
INTEGER(I4B) :: nr,nc

nodes = RefCoord_Quadrangle("BIUNIT")
points = nodes
CALL LagrangeEvalAll_Quadrangle_(order=1_I4B,x=points,xij=nodes, &
  basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
  < 1.0E-11_DFP, "Partition of unity")
CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
  ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
END PROGRAM main
