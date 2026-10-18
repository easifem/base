!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordBiUnitTriangle for points in biunit triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordBiUnitTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 5), ans(3, 5), eye(3, 3)

! vertices, centroid and mid point of edge 1-2
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
               -third, -third, 0.0_DFP, -1.0_DFP], [2_I4B, 5_I4B])
eye = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, &
               0.0_DFP, 0.0_DFP, 1.0_DFP], [3_I4B, 3_I4B])

ans = BarycentricCoordBiUnitTriangle(xin)
CALL OK(ALL(ABS(ans(:, 1:3) - eye) .LT. tol), "vertices")
CALL OK(ALL(ABS(ans(:, 4) - third) .LT. tol), "centroid")
CALL OK(ALL(ABS(ans(:, 5) - [0.5_DFP, 0.5_DFP, 0.0_DFP]) .LT. tol), &
        "mid point of edge 1-2")
CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")

END PROGRAM main
