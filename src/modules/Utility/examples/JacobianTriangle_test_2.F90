!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: JacobianTriangle with lower case names and clockwise vertices
!
! Description:
!   Regression test for bugs M-20 and M-25 in
!   bug-reports/Utility_bug_report.md.
!   The names were upper case only, and in 2D the area was signed, so
!   clockwise vertices gave a negative Jacobian.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xcw(2, 3)

! the case does not matter
CALL OK(ABS(JacobianTriangle("unit", "biunit") - 4.0_DFP) .LT. tol, &
        "unit to biunit")

! (0,0), (0,3), (2,0) is clockwise, the area is 3
xcw(:, 1) = [0.0_DFP, 0.0_DFP]
xcw(:, 2) = [0.0_DFP, 3.0_DFP]
xcw(:, 3) = [2.0_DFP, 0.0_DFP]
CALL OK(ABS(JacobianTriangle("UNIT", "TRIANGLE", xcw) - 6.0_DFP) &
        .LT. tol, "clockwise: UNIT to TRIANGLE is positive")
CALL OK(ABS(JacobianTriangle("TRIANGLE", "BIUNIT", xcw) &
            - 2.0_DFP / 3.0_DFP) .LT. tol, &
        "clockwise: TRIANGLE to BIUNIT is positive")

END PROGRAM main
