!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitTetrahedron2Tetrahedron, biunit to a tetrahedron

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromBiUnitTetrahedron2Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(3, 5), x1(3), x2(3), x3(3), x4(3), ans(3, 5)

! vertices and centroid of the biunit tetrahedron
xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
xb(:, 5) = -0.5_DFP

! vertices of a tetrahedron
x1 = [1.0_DFP, 1.0_DFP, 1.0_DFP]
x2 = [3.0_DFP, 1.0_DFP, 1.0_DFP]
x3 = [1.0_DFP, 4.0_DFP, 1.0_DFP]
x4 = [1.0_DFP, 1.0_DFP, 5.0_DFP]

ans = FromBiUnitTetrahedron2Tetrahedron(xb, x1, x2, x3, x4)
CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "vertex 1 goes to x1")
CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "vertex 2 goes to x2")
CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "vertex 3 goes to x3")
CALL OK(ALL(ABS(ans(:, 4) - x4) .LT. tol), "vertex 4 goes to x4")
CALL OK(ALL(ABS(ans(:, 5) - [1.5_DFP, 1.75_DFP, 2.0_DFP]) .LT. tol), &
        "centroid goes to centroid")

END PROGRAM main
