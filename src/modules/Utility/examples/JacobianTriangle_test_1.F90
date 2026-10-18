!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: JacobianTriangle for reference and general triangles
!
! Description:
!   Includes the regression test for bug H-13 in
!   bug-reports/Utility_bug_report.md: the Jacobian of the map from
!   a triangle to itself was its area instead of 1.

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xij(2, 3), xij3(3, 3)

! between the reference triangles
CALL OK(ABS(JacobianTriangle("UNIT", "BIUNIT") - 4.0_DFP) .LT. tol, &
        "UNIT to BIUNIT")
CALL OK(ABS(JacobianTriangle("BIUNIT", "UNIT") - 0.25_DFP) .LT. tol, &
        "BIUNIT to UNIT")
CALL OK(ABS(JacobianTriangle("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
        "UNIT to UNIT")

! a triangle with vertices (0,0), (2,0), (0,3), its area is 3
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [2.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 3.0_DFP]
CALL OK(ABS(JacobianTriangle("UNIT", "TRIANGLE", xij) - 6.0_DFP) &
        .LT. tol, "UNIT to TRIANGLE")
CALL OK(ABS(JacobianTriangle("BIUNIT", "TRIANGLE", xij) - 1.5_DFP) &
        .LT. tol, "BIUNIT to TRIANGLE")
CALL OK(ABS(JacobianTriangle("TRIANGLE", "UNIT", xij) - 1.0_DFP / 6.0_DFP) &
        .LT. tol, "TRIANGLE to UNIT")
CALL OK(ABS(JacobianTriangle("TRIANGLE", "BIUNIT", xij) - &
            2.0_DFP / 3.0_DFP) .LT. tol, "TRIANGLE to BIUNIT")

! the same triangle in the plane z = 1
xij3(1:2, :) = xij
xij3(3, :) = 1.0_DFP
CALL OK(ABS(JacobianTriangle("UNIT", "TRIANGLE", xij3) - 6.0_DFP) &
        .LT. tol, "UNIT to TRIANGLE in 3D")

! a map from a triangle to itself has Jacobian 1
CALL OK(ABS(JacobianTriangle("TRIANGLE", "TRIANGLE", xij) - 1.0_DFP) &
        .LT. tol, "TRIANGLE to TRIANGLE")
CALL OK(ABS(JacobianTriangle("TRIANGLE", "TRIANGLE", xij3) - 1.0_DFP) &
        .LT. tol, "TRIANGLE to TRIANGLE in 3D")

END PROGRAM main
