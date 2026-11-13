!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromLine2Line_ with mixed case names
!
! Description:
!   Regression test for bug M-21 in bug-reports/Utility_bug_report.md.
!   Some mixed-case combinations, such as "Bb", were not accepted.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromLine2Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xin(3), ans(3)

xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]

CALL FromLine2Line_(xin=xin, ans=ans, from="Biunit", to="unit")
CALL OK(ALL(ABS(ans - [0.0_DFP, 0.5_DFP, 1.0_DFP]) .LT. tol), &
        "Biunit to unit")

ans = 9.0_DFP
CALL FromLine2Line_(xin=xin, ans=ans, from="B", to="b")
CALL OK(ALL(ABS(ans - xin) .LT. tol), "B to b copies xin")

ans = 9.0_DFP
CALL FromLine2Line_(xin=[0.0_DFP, 1.0_DFP], ans=ans(1:2), from="u", &
                    to="B")
CALL OK(ALL(ABS(ans(1:2) - [-1.0_DFP, 1.0_DFP]) .LT. tol), "u to B")

! an unknown name does not change ans
ans = 9.0_DFP
CALL FromLine2Line_(xin=xin, ans=ans, from="X", to="B")
CALL OK(ALL(ans .EQ. 9.0_DFP), "unknown name")

END PROGRAM main
