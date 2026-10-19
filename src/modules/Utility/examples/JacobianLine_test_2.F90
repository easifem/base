!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: JacobianLine with lower case names, unknown names and no xij
!
! Description:
!   Regression test for bug M-20 in bug-reports/Utility_bug_report.md.
!   The names were upper case only, an unknown name left the result
!   undefined, and "LINE" used xij without checking that it is present.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianLine
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! the case does not matter
CALL OK(ABS(JacobianLine("biunit", "unit") - 0.5_DFP) .LT. tol, &
        "biunit to unit")
CALL OK(ABS(JacobianLine("Unit", "BiUnit") - 2.0_DFP) .LT. tol, &
        "Unit to BiUnit")

! LINE without xij gives 1
CALL OK(ABS(JacobianLine("UNIT", "LINE") - 1.0_DFP) .LT. tol, &
        "LINE without xij")

! an unknown name gives 1
CALL OK(ABS(JacobianLine("ARC", "UNIT") - 1.0_DFP) .LT. tol, &
        "unknown name")

END PROGRAM main
