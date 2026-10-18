!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: JacobianLine for reference lines and a general segment

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianLine
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xij(2, 2)

! between the reference lines
CALL OK(ABS(JacobianLine("UNIT", "BIUNIT") - 2.0_DFP) .LT. tol, &
        "UNIT to BIUNIT")
CALL OK(ABS(JacobianLine("BIUNIT", "UNIT") - 0.5_DFP) .LT. tol, &
        "BIUNIT to UNIT")
CALL OK(ABS(JacobianLine("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
        "BIUNIT to BIUNIT")
CALL OK(ABS(JacobianLine("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
        "UNIT to UNIT")

! a segment from (0, 0) to (3, 4), its length is 5
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [3.0_DFP, 4.0_DFP]
CALL OK(ABS(JacobianLine("BIUNIT", "LINE", xij) - 2.5_DFP) .LT. tol, &
        "BIUNIT to LINE")
CALL OK(ABS(JacobianLine("UNIT", "LINE", xij) - 5.0_DFP) .LT. tol, &
        "UNIT to LINE")
CALL OK(ABS(JacobianLine("LINE", "BIUNIT", xij) - 0.4_DFP) .LT. tol, &
        "LINE to BIUNIT")
CALL OK(ABS(JacobianLine("LINE", "UNIT", xij) - 0.2_DFP) .LT. tol, &
        "LINE to UNIT")

END PROGRAM main
