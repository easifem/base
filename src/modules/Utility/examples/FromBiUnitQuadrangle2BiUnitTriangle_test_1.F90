!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2BiUnitTriangle, square to biunit triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2BiUnitTriangle
USE MappingUtility, ONLY: FromBiUnitSqr2BiUnitTriangle
USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)

! vertices of the biunit square and the image of the centroid
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
               -1.0_DFP, 1.0_DFP, 0.0_DFP, -third], [2_I4B, 5_I4B])

! the top edge of the square collapses to vertex 3, (-1, 1)
expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                    -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, &
                    -third, -third], [2_I4B, 5_I4B])

ans = FromBiUnitQuadrangle2BiUnitTriangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")

ans = FromBiUnitSqr2BiUnitTriangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitSqr2BiUnitTriangle")

! inverse of FromBiUnitTriangle2BiUnitQuadrangle (away from vertex 3)
ans(:, 1:2) = FromBiUnitTriangle2BiUnitQuadrangle( &
              FromBiUnitQuadrangle2BiUnitTriangle(xin(:, [2, 5])))
CALL OK(ALL(ABS(ans(:, 1:2) - xin(:, [2, 5])) .LT. tol), "round trip")

END PROGRAM main
