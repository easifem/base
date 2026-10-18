!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitTriangle2BiUnitQuadrangle, biunit triangle to square

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitQuadrangle
USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitSqr
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 4), ans(2, 4), expected(2, 4)

! vertex 1, vertex 2, centroid and mid point of edge 1-2
! vertex 3, (-1, 1), is the collapsed point and is not used here
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third, &
               0.0_DFP, -1.0_DFP], [2_I4B, 4_I4B])
expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                    0.0_DFP, -third, 0.0_DFP, -1.0_DFP], [2_I4B, 4_I4B])

ans = FromBiUnitTriangle2BiUnitQuadrangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")

! the specific name gives the same result
ans = FromBiUnitTriangle2BiUnitSqr(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitTriangle2BiUnitSqr")

END PROGRAM main
