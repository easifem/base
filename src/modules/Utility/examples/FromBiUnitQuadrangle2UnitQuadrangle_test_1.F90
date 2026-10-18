!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2UnitQuadrangle maps [-1,1]^2 to [0,1]^2

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)

! vertices and centre of the biunit quadrangle
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
               -1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [2_I4B, 5_I4B])

! vertices and centre of the unit quadrangle
expected = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, &
                    0.0_DFP, 1.0_DFP, 0.5_DFP, 0.5_DFP], [2_I4B, 5_I4B])

ans = FromBiUnitQuadrangle2UnitQuadrangle(xin)
CALL OK(ALL(ABS(ans - expected) .LT. tol), "vertices and centre")

! the map is (1 + x) / 2 in each direction
CALL OK(ALL(ABS(ans - 0.5_DFP * (1.0_DFP + xin)) .LT. tol), &
        "ans = (1 + xin) / 2")

END PROGRAM main
