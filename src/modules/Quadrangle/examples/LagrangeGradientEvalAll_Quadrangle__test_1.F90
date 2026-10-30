!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE QuadrangleInterpolationUtility, &
  ONLY: LagrangeGradientEvalAll_Quadrangle_
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,4),points(2,4),y(4,4,2)
INTEGER(I4B) :: nr,nc,nd

nodes = RefCoord_Quadrangle("BIUNIT")
points = nodes
CALL LagrangeGradientEvalAll_Quadrangle_(order=1_I4B,x=points, &
  xij=nodes,basisType=poly%Monomial,ans=y,dim1=nr,dim2=nc,dim3=nd)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
  < 1.0E-11_DFP, "Gradient of partition of unity")
END PROGRAM main
