!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromUnitTriangle2BiUnitQuadrangle at vertex 3
!
! Description:
!   Regression test for bug M-23 in bug-reports/Utility_bug_report.md.
!   Vertex 3 is the collapsed point, and the map divided by zero there.
!   Now vertex 3 goes to (-1, 1).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitTriangle2BiUnitQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 1), ans(2, 1)

xin = RESHAPE([0.0_DFP, 1.0_DFP], [2_I4B, 1_I4B])
ans = FromUnitTriangle2BiUnitQuadrangle(xin)
CALL OK(ALL(ABS(ans(:, 1) - [-1.0_DFP, 1.0_DFP]) .LT. tol), &
        "vertex 3 goes to (-1, 1)")

END PROGRAM main
