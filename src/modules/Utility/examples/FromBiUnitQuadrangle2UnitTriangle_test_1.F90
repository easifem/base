!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2UnitTriangle, square to unit triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitTriangle
USE MappingUtility, ONLY: FromBiUnitSqr2UnitTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)

! vertices of the biunit square and the image of the centroid
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
               -1.0_DFP, 1.0_DFP, 0.0_DFP, -third], [2_I4B, 5_I4B])

! the top edge of the square collapses to vertex 3, (0, 1)
expected = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
                    0.0_DFP, 1.0_DFP, third, third], [2_I4B, 5_I4B])

ans = FromBiUnitQuadrangle2UnitTriangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")

ans = FromBiUnitSqr2UnitTriangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitSqr2UnitTriangle")

END PROGRAM main
