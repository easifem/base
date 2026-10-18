!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: JacobianHexahedron for reference and general hexahedrons

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianHexahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xij(3, 8)

! between the reference hexahedrons
CALL OK(ABS(JacobianHexahedron("UNIT", "BIUNIT") - 8.0_DFP) .LT. tol, &
        "UNIT to BIUNIT")
CALL OK(ABS(JacobianHexahedron("BIUNIT", "UNIT") - 0.125_DFP) .LT. tol, &
        "BIUNIT to UNIT")
CALL OK(ABS(JacobianHexahedron("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
        "BIUNIT to BIUNIT")
CALL OK(ABS(JacobianHexahedron("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
        "UNIT to UNIT")

! the box [0,2] x [0,3] x [0,4], its volume is 24
xij(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xij(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
xij(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
xij(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
xij(:, 5:8) = xij(:, 1:4)
xij(3, 5:8) = 4.0_DFP
CALL OK(ABS(JacobianHexahedron("BIUNIT", "HEXAHEDRON", xij) - 3.0_DFP) &
        .LT. tol, "BIUNIT to HEXAHEDRON")
CALL OK(ABS(JacobianHexahedron("UNIT", "HEXAHEDRON", xij) - 24.0_DFP) &
        .LT. tol, "UNIT to HEXAHEDRON")
CALL OK(ABS(JacobianHexahedron("HEXAHEDRON", "BIUNIT", xij) - &
            1.0_DFP / 3.0_DFP) .LT. tol, "HEXAHEDRON to BIUNIT")
CALL OK(ABS(JacobianHexahedron("HEXAHEDRON", "UNIT", xij) - &
            1.0_DFP / 24.0_DFP) .LT. tol, "HEXAHEDRON to UNIT")

END PROGRAM main
