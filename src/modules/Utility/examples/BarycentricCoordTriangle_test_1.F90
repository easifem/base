!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordTriangle for unit and biunit triangles

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordTriangle
USE MappingUtility, ONLY: BarycentricCoordUnitTriangle
USE MappingUtility, ONLY: BarycentricCoordBiUnitTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xu(2, 2), xb(2, 2), ans(3, 2)

xu = RESHAPE([0.2_DFP, 0.3_DFP, 0.5_DFP, 0.0_DFP], [2_I4B, 2_I4B])
xb = RESHAPE([-0.6_DFP, -0.4_DFP, 0.0_DFP, -1.0_DFP], [2_I4B, 2_I4B])

ans = BarycentricCoordTriangle(xu, "UNIT")
CALL OK(ALL(ABS(ans - BarycentricCoordUnitTriangle(xu)) .LT. tol), &
        "UNIT")
CALL OK(ALL(ABS(ans(:, 1) - [0.5_DFP, 0.2_DFP, 0.3_DFP]) .LT. tol), &
        "UNIT, point (0.2, 0.3)")

ans = BarycentricCoordTriangle(xb, "BIUNIT")
CALL OK(ALL(ABS(ans - BarycentricCoordBiUnitTriangle(xb)) .LT. tol), &
        "BIUNIT")

! only the first letter is used, lower case is accepted
ans = BarycentricCoordTriangle(xu, "unit")
CALL OK(ALL(ABS(ans - BarycentricCoordUnitTriangle(xu)) .LT. tol), &
        "lower case")

END PROGRAM main
