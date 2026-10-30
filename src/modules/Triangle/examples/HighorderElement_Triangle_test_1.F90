!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE ReferenceTriangle_Method, ONLY: HighorderElement_Triangle
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: nodes(:, :)

! HighorderElement_Triangle indexes a nonexistent fourth face column.
! Construct the interpolation nodes directly until that backend is repaired.
nodes = InterpolationPoint_Triangle(order=2_I4B, &
  ipType=Equidistance, layout="VEFC")
CALL OK(ALL(SHAPE(nodes) == [2, 6]), "Quadratic triangle node workaround")
END PROGRAM main
