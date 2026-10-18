!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitHexahedron2BiUnitHexahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitHexahedron2BiUnitHexahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 2), ans(4, 4)
INTEGER(I4B) :: nrow, ncol

xin(:, 1) = 0.5_DFP
xin(:, 2) = [0.25_DFP, 0.5_DFP, 0.75_DFP]

CALL FromUnitHexahedron2BiUnitHexahedron_(xin, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(xin, 1)")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1)) .LT. tol), "centre goes to (0,0,0)")
CALL OK(ALL(ABS(ans(1:3, 2) - [-0.5_DFP, 0.0_DFP, 0.5_DFP]) .LT. tol), &
        "interior point")

END PROGRAM main
