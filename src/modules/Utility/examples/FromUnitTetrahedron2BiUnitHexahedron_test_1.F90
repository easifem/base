!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitTetrahedron2BiUnitHexahedron, collapsed coordinates

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromUnitTetrahedron2BiUnitHexahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xu(3, 5), ans(3, 5), expected(3, 5)

! vertices and centroid of the unit tetrahedron
xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
xu(:, 5) = 0.25_DFP

expected(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
expected(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
expected(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
expected(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
expected(:, 5) = [0.0_DFP, -third, -0.5_DFP]

ans = FromUnitTetrahedron2BiUnitHexahedron(xu)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "vertices and centroid")

END PROGRAM main
