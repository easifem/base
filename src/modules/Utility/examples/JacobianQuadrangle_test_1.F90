!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: JacobianQuadrangle for reference and general quadrangles
!
! Description:
!   Includes the regression test for bug H-13 in
!   bug-reports/Utility_bug_report.md: the Jacobian of the map from
!   a quadrangle to itself was its area instead of 1.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xij(2, 4)

! between the reference quadrangles
CALL OK(ABS(JacobianQuadrangle("UNIT", "BIUNIT") - 4.0_DFP) .LT. tol, &
        "UNIT to BIUNIT")
CALL OK(ABS(JacobianQuadrangle("BIUNIT", "UNIT") - 0.25_DFP) .LT. tol, &
        "BIUNIT to UNIT")
CALL OK(ABS(JacobianQuadrangle("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
        "BIUNIT to BIUNIT")

! the rectangle [0,2] x [0,3], its area is 6
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [2.0_DFP, 0.0_DFP]
xij(:, 3) = [2.0_DFP, 3.0_DFP]
xij(:, 4) = [0.0_DFP, 3.0_DFP]
CALL OK(ABS(JacobianQuadrangle("BIUNIT", "QUADRANGLE", xij) - 1.5_DFP) &
        .LT. tol, "BIUNIT to QUADRANGLE")
CALL OK(ABS(JacobianQuadrangle("UNIT", "QUADRANGLE", xij) - 6.0_DFP) &
        .LT. tol, "UNIT to QUADRANGLE")
CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "BIUNIT", xij) - &
            4.0_DFP / 6.0_DFP) .LT. tol, "QUADRANGLE to BIUNIT")
CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "UNIT", xij) - &
            1.0_DFP / 6.0_DFP) .LT. tol, "QUADRANGLE to UNIT")

! a map from a quadrangle to itself has Jacobian 1
CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "QUADRANGLE", xij) &
            - 1.0_DFP) .LT. tol, "QUADRANGLE to QUADRANGLE")

END PROGRAM main
