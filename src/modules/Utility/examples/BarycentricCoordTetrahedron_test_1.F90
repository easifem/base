!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordTetrahedron for unit and biunit tetrahedrons

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: BarycentricCoordTetrahedron
USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xu(3, 2), xb(3, 2), ans(4, 2)

xu(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
xu(:, 2) = 0.25_DFP
xb(:, 1) = [-0.8_DFP, -0.6_DFP, -0.4_DFP]
xb(:, 2) = -0.5_DFP

ans = BarycentricCoordTetrahedron(xu, "UNIT")
CALL OK(ALL(ABS(ans - BarycentricCoordUnitTetrahedron(xu)) .LT. tol), &
        "UNIT")
CALL OK(ALL(ABS(ans(:, 1) - [0.4_DFP, 0.1_DFP, 0.2_DFP, 0.3_DFP]) &
            .LT. tol), "UNIT, point (0.1, 0.2, 0.3)")

ans = BarycentricCoordTetrahedron(xb, "BIUNIT")
CALL OK(ALL(ABS(ans - BarycentricCoordBiUnitTetrahedron(xb)) .LT. tol), &
        "BIUNIT")

! only the first letter is used, lower case is accepted
ans = BarycentricCoordTetrahedron(xb, "biunit")
CALL OK(ALL(ABS(ans(:, 2) - 0.25_DFP) .LT. tol), "lower case")

END PROGRAM main
