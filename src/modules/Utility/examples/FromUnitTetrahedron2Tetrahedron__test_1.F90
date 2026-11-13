!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitTetrahedron2Tetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitTetrahedron2Tetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 2), x1(3), x2(3), x3(3), x4(3), ans(4, 4)
INTEGER(I4B) :: nrow, ncol

! centroid and vertex 4 of the unit tetrahedron
xin(:, 1) = 0.25_DFP
xin(:, 2) = [0.0_DFP, 0.0_DFP, 1.0_DFP]

x1 = [1.0_DFP, 1.0_DFP, 1.0_DFP]
x2 = [3.0_DFP, 1.0_DFP, 1.0_DFP]
x3 = [1.0_DFP, 4.0_DFP, 1.0_DFP]
x4 = [1.0_DFP, 1.0_DFP, 5.0_DFP]

CALL FromUnitTetrahedron2Tetrahedron_(xin, x1, x2, x3, x4, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1) - [1.5_DFP, 1.75_DFP, 2.0_DFP]) .LT. tol), &
        "centroid")
CALL OK(ALL(ABS(ans(1:3, 2) - x4) .LT. tol), "vertex 4")

END PROGRAM main
