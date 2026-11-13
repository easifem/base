!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: JacobianHexahedron with lower case names
!
! Description:
!   Regression test for bug M-20 in bug-reports/Utility_bug_report.md.
!   The names were upper case only.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianHexahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

CALL OK(ABS(JacobianHexahedron("biunit", "unit") - 0.125_DFP) .LT. tol, &
        "biunit to unit")

END PROGRAM main
