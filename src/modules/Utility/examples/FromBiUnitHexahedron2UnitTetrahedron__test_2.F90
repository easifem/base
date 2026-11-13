!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromBiUnitHexahedron2UnitTetrahedron_ with a larger ans
!
! Description:
!   Regression test for bug H-12 in bug-reports/Utility_bug_report.md.
!   The second step was called with ans as both input and output,
!   which is not allowed, and it set nrow and ncol to the size of ans.
!   With a larger ans, nrow and ncol were wrong and entries outside
!   the result were changed.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron
USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP), PARAMETER :: fill = -7.0_DFP
REAL(DFP) :: xh(3, 3), ans(4, 5), want(3, 3)
INTEGER(I4B) :: nrow, ncol

xh(:, 1) = [0.0_DFP, -third, -0.5_DFP]
xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xh(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
want = FromBiUnitHexahedron2UnitTetrahedron(xh)

ans = fill
CALL FromBiUnitHexahedron2UnitTetrahedron_(xh, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
CALL OK(ncol .EQ. 3_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1:3) - want) .LT. tol), &
        "same as FromBiUnitHexahedron2UnitTetrahedron")
CALL OK(ALL(ABS(ans(1:3, 1) - 0.25_DFP) .LT. tol), "centroid")
CALL OK(ALL(ans(4, :) .EQ. fill), "row 4 is not changed")
CALL OK(ALL(ans(:, 4:5) .EQ. fill), "columns 4 and 5 are not changed")

END PROGRAM main
