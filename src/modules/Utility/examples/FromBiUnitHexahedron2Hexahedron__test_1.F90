!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitHexahedron2Hexahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitHexahedron2Hexahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 2), xv(3, 8), ans(4, 5)
INTEGER(I4B) :: nrow, ncol

! centre and vertex 7 of the biunit hexahedron
xin(:, 1) = 0.0_DFP
xin(:, 2) = 1.0_DFP

! vertices of the box [0,2] x [0,3] x [0,4]
xv(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xv(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
xv(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
xv(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
xv(:, 5:8) = xv(:, 1:4)
xv(3, 5:8) = 4.0_DFP

CALL FromBiUnitHexahedron2Hexahedron_(xin, xv(:, 1), xv(:, 2), xv(:, 3), &
                                      xv(:, 4), xv(:, 5), xv(:, 6), &
                                      xv(:, 7), xv(:, 8), ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(x1)")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1) - [1.0_DFP, 1.5_DFP, 2.0_DFP]) .LT. tol), &
        "centre")
CALL OK(ALL(ABS(ans(1:3, 2) - xv(:, 7)) .LT. tol), "vertex 7")

END PROGRAM main
