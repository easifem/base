!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: JacobianTetrahedron for reference and general tetrahedrons

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: JacobianTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xij(3, 4)

! between the reference tetrahedrons
CALL OK(ABS(JacobianTetrahedron("UNIT", "BIUNIT") - 8.0_DFP) .LT. tol, &
        "UNIT to BIUNIT")
CALL OK(ABS(JacobianTetrahedron("BIUNIT", "UNIT") - 0.125_DFP) .LT. tol, &
        "BIUNIT to UNIT")
CALL OK(ABS(JacobianTetrahedron("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
        "UNIT to UNIT")

! vertices (0,0,0), (2,0,0), (0,3,0), (0,0,4), the volume is 4
xij = 0.0_DFP
xij(1, 2) = 2.0_DFP
xij(2, 3) = 3.0_DFP
xij(3, 4) = 4.0_DFP
CALL OK(ABS(JacobianTetrahedron("UNIT", "TETRAHEDRON", xij) - 24.0_DFP) &
        .LT. tol, "UNIT to TETRAHEDRON")
CALL OK(ABS(JacobianTetrahedron("BIUNIT", "TETRAHEDRON", xij) - 3.0_DFP) &
        .LT. tol, "BIUNIT to TETRAHEDRON")
CALL OK(ABS(JacobianTetrahedron("TETRAHEDRON", "UNIT", xij) - &
            1.0_DFP / 24.0_DFP) .LT. tol, "TETRAHEDRON to UNIT")
CALL OK(ABS(JacobianTetrahedron("TETRAHEDRON", "BIUNIT", xij) - &
            1.0_DFP / 3.0_DFP) .LT. tol, "TETRAHEDRON to BIUNIT")

END PROGRAM main
