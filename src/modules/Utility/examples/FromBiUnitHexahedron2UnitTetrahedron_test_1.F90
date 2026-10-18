!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitHexahedron2UnitTetrahedron, cube to unit tetrahedron

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xh(3, 9), ans(3, 9)

! vertices of the biunit hexahedron
xh(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xh(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
xh(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xh(:, 5:8) = xh(:, 1:4)
xh(3, 5:8) = 1.0_DFP
! image of the centroid of the biunit tetrahedron
xh(:, 9) = [0.0_DFP, -third, -0.5_DFP]

ans = FromBiUnitHexahedron2UnitTetrahedron(xh)
CALL OK(ALL(ABS(ans(:, 1)) .LT. tol), "vertex 1 goes to (0,0,0)")
CALL OK(ALL(ABS(ans(:, 2) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) .LT. tol), &
        "vertex 2 goes to (1,0,0)")
CALL OK(ALL(ABS(ans(:, 3) - [0.0_DFP, 1.0_DFP, 0.0_DFP]) .LT. tol), &
        "vertex 3 goes to (0,1,0)")
CALL OK(ALL(ABS(ans(:, 7) - [0.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
        "vertex 7 goes to (0,0,1)")
CALL OK(ALL(ABS(ans(:, 9) - 0.25_DFP) .LT. tol), "centroid")

END PROGRAM main
