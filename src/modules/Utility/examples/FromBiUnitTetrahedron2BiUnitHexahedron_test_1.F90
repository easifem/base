!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitTetrahedron2BiUnitHexahedron, collapsed coordinates

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromBiUnitTetrahedron2BiUnitHexahedron
USE MappingUtility, ONLY: FromBiUnitHexahedron2BiUnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xb(3, 5), ans(3, 5), xh(3, 1)

! vertices and centroid of the biunit tetrahedron
xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
xb(:, 5) = -0.5_DFP

ans = FromBiUnitTetrahedron2BiUnitHexahedron(xb)
! vertices 1 and 2 are not changed
CALL OK(ALL(ABS(ans(:, 1:2) - xb(:, 1:2)) .LT. tol), "vertices 1 and 2")
! at the collapsed points, ans(1) = -1 (and ans(2) = -1 at vertex 4)
CALL OK(ALL(ABS(ans(:, 3) - xb(:, 3)) .LT. tol), "vertex 3")
CALL OK(ALL(ABS(ans(:, 4) - xb(:, 4)) .LT. tol), "vertex 4")
CALL OK(ALL(ABS(ans(:, 5) - [0.0_DFP, -third, -0.5_DFP]) .LT. tol), &
        "centroid")

! inverse of FromBiUnitHexahedron2BiUnitTetrahedron, inside the cube
xh(:, 1) = [0.2_DFP, -0.4_DFP, 0.3_DFP]
ans(:, 1:1) = FromBiUnitTetrahedron2BiUnitHexahedron( &
              FromBiUnitHexahedron2BiUnitTetrahedron(xh))
CALL OK(ALL(ABS(ans(:, 1:1) - xh) .LT. tol), "round trip")

END PROGRAM main
