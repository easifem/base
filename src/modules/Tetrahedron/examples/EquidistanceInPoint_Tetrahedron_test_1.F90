!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: EquidistanceInPoint_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: nodes(:,:)

nodes = EquidistanceInPoint_Tetrahedron(order=4_I4B)
CALL OK(ALL(SHAPE(nodes) == [3,1]), &
  "Interpolation coordinate dimensions")
CALL OK(MAXVAL(ABS(nodes-(0.25_DFP))) &
  < 1.0E-11_DFP, "Single interior point is the centroid")
END PROGRAM main
