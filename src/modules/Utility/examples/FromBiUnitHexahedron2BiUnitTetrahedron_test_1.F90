!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitHexahedron2BiUnitTetrahedron, collapse the cube

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitHexahedron2BiUnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xh(3, 9), xb(3, 4), ans(3, 9)
INTEGER(I4B) :: ii

! vertices of the biunit hexahedron
xh(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xh(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
xh(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xh(:, 5:8) = xh(:, 1:4)
xh(3, 5:8) = 1.0_DFP
! image of the centroid of the biunit tetrahedron
xh(:, 9) = [0.0_DFP, -third, -0.5_DFP]

! vertices of the biunit tetrahedron
xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]

ans = FromBiUnitHexahedron2BiUnitTetrahedron(xh)
CALL OK(ALL(ABS(ans(:, 1:2) - xb(:, 1:2)) .LT. tol), "vertices 1 and 2")
! the edge 3-4 of the cube collapses to vertex 3
CALL OK(ALL(ABS(ans(:, 3) - xb(:, 3)) .LT. tol), "vertex 3 of cube")
CALL OK(ALL(ABS(ans(:, 4) - xb(:, 3)) .LT. tol), "vertex 4 of cube")
! the top face of the cube collapses to vertex 4
DO ii = 5_I4B, 8_I4B
  CALL OK(ALL(ABS(ans(:, ii) - xb(:, 4)) .LT. tol), "top face of cube")
END DO
CALL OK(ALL(ABS(ans(:, 9) + 0.5_DFP) .LT. tol), "centroid")

END PROGRAM main
