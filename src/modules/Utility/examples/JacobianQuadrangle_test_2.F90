!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: JacobianQuadrangle with lower case names and clockwise vertices
!
! Description:
!   Regression test for bugs M-20 and M-25 in
!   bug-reports/Utility_bug_report.md.
!   The names were upper case only, and in 2D the area was signed, so
!   clockwise vertices gave a negative Jacobian.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xcw(2, 4)

! the case does not matter
CALL OK(ABS(JacobianQuadrangle("biunit", "unit") - 0.25_DFP) .LT. tol, &
        "biunit to unit")

! the rectangle [0,2] x [0,3] in clockwise order, area 6
xcw(:, 1) = [0.0_DFP, 0.0_DFP]
xcw(:, 2) = [0.0_DFP, 3.0_DFP]
xcw(:, 3) = [2.0_DFP, 3.0_DFP]
xcw(:, 4) = [2.0_DFP, 0.0_DFP]
CALL OK(ABS(JacobianQuadrangle("BIUNIT", "QUADRANGLE", xcw) - 1.5_DFP) &
        .LT. tol, "clockwise: BIUNIT to QUADRANGLE is positive")

END PROGRAM main
