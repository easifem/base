!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE QuadrangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,4),points(2,4),y(4,4,2)

nodes = RefCoord_Quadrangle("BIUNIT")
points = nodes
y = LagrangeGradientEvalAll_Quadrangle(order=1_I4B,x=points,xij=nodes, &
  basisType=poly%Monomial)
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
  < 1.0E-11_DFP, "Gradient of partition of unity")
END PROGRAM main
