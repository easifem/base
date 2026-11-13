!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitHexahedron2UnitTetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xh(3, 2), ans(3, 2)
INTEGER(I4B) :: nrow, ncol

xh(:, 1) = [0.0_DFP, -third, -0.5_DFP]
xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]

! ans must have exactly 3 rows and SIZE(xin, 2) columns
CALL FromBiUnitHexahedron2UnitTetrahedron_(xh, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1) - 0.25_DFP) .LT. tol), "centroid")
CALL OK(ALL(ABS(ans(1:3, 2) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) .LT. tol), &
        "vertex 2")

END PROGRAM main
