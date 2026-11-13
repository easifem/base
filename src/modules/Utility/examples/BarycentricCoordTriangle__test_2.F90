!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: BarycentricCoordTriangle_ with a larger ans and lower case
!
! Description:
!   Regression test for bug M-24 in bug-reports/Utility_bug_report.md.
!   ans had to have exactly SIZE(xin, 2) columns, and the name had to
!   be upper case.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordTriangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 1), ans(4, 3)

ans = 9.0_DFP

! the centroid of the biunit triangle
xin = RESHAPE([-1.0_DFP, -1.0_DFP] / 3.0_DFP, [2_I4B, 1_I4B])
CALL BarycentricCoordTriangle_(xin, "biunit", ans)
CALL OK(ALL(ABS(ans(1:3, 1) - 1.0_DFP / 3.0_DFP) .LT. tol), "centroid")
CALL OK(ans(4, 1) .EQ. 9.0_DFP, "row 4 is not changed")
CALL OK(ALL(ans(:, 2:3) .EQ. 9.0_DFP), "extra columns are not changed")

END PROGRAM main
