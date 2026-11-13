!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitHexahedron2Hexahedron maps [-1,1]^3 to a hexahedron

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromBiUnitHexahedron2Hexahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 9), xv(3, 8), ans(3, 9)

! vertices of the biunit hexahedron (bottom face, then top face)
xin(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xin(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xin(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
xin(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xin(:, 5:8) = xin(:, 1:4)
xin(3, 5:8) = 1.0_DFP
! centre
xin(:, 9) = 0.0_DFP

! vertices of the box [0,2] x [0,3] x [0,4]
xv(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xv(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
xv(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
xv(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
xv(:, 5:8) = xv(:, 1:4)
xv(3, 5:8) = 4.0_DFP

ans = FromBiUnitHexahedron2Hexahedron(xin, xv(:, 1), xv(:, 2), xv(:, 3), &
                                      xv(:, 4), xv(:, 5), xv(:, 6), &
                                      xv(:, 7), xv(:, 8))
CALL OK(ALL(ABS(ans(:, 1:8) - xv) .LT. tol), &
        "vertices go to vertices")
CALL OK(ALL(ABS(ans(:, 9) - [1.0_DFP, 1.5_DFP, 2.0_DFP]) .LT. tol), &
        "centre goes to centre")

END PROGRAM main
