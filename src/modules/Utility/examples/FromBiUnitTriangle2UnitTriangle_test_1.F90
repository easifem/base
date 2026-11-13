!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitTriangle2UnitTriangle, biunit to unit triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitTriangle2UnitTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xb(2, 4), xu(2, 4), ans(2, 4)

! vertices and centroid
xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
              -third, -third], [2_I4B, 4_I4B])
xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
              third, third], [2_I4B, 4_I4B])

ans = FromBiUnitTriangle2UnitTriangle(xb)
CALL OK(ALL(ABS(ans - xu) .LT. tol), "vertices and centroid")

END PROGRAM main
