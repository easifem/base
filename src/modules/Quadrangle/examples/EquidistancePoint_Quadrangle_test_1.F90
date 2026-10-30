!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: nodes(:,:)

nodes = EquidistancePoint_Quadrangle(order=2_I4B)
CALL OK(ALL(SHAPE(nodes) == [2,9]), &
  "Interpolation coordinate dimensions")
CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
END PROGRAM main
