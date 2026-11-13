!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromTriangle2Square_ with mixed case, vertex 3 and a larger ans
!
! Description:
!   Regression test for bugs M-21, M-23 and M-24 in
!   bug-reports/Utility_bug_report.md.
!   Mixed-case names were not accepted, vertex 3 divided by zero, and
!   ans had to have exactly SIZE(xin, 2) columns.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromTriangle2Square_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(2, 2), xu(2, 2), ans(2, 4), expected(2, 2)

! vertex 2 and vertex 3 of the biunit and unit triangles
xb = RESHAPE([1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP], [2_I4B, 2_I4B])
xu = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], [2_I4B, 2_I4B])
expected = RESHAPE([1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP], &
                   [2_I4B, 2_I4B])

! ans has more columns than xin; the extra columns are not changed
ans = 9.0_DFP
CALL FromTriangle2Square_(xin=xb, ans=ans, from="bI", to="Bi")
CALL OK(ALL(ABS(ans(:, 1:2) - expected) .LT. tol), &
        "bI to Bi, vertex 3 goes to (-1, 1)")
CALL OK(ALL(ans(:, 3:4) .EQ. 9.0_DFP), "extra columns are not changed")

ans = 9.0_DFP
CALL FromTriangle2Square_(xin=xu, ans=ans, from="unit", to="biunit")
CALL OK(ALL(ABS(ans(:, 1:2) - expected) .LT. tol), &
        "unit to biunit, vertex 3 goes to (-1, 1)")

END PROGRAM main
