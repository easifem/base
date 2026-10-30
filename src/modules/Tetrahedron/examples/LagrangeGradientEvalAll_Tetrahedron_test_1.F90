!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE TetrahedronInterpolationUtility, &
  ONLY: LagrangeGradientEvalAll_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(3,4),points(3,4),y(4,4,3)

nodes = RefCoord_Tetrahedron("UNIT")
points = nodes
y = LagrangeGradientEvalAll_Tetrahedron(order=1_I4B,x=points, &
  xij=nodes,basisType=poly%Monomial,refTetrahedron="UNIT")
CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
  < 1.0E-11_DFP, "Gradient of partition of unity")
END PROGRAM main
