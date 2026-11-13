!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitQuadrangle2BiUnitQuadrangle maps [0,1]^2 to [-1,1]^2

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitQuadrangle2BiUnitQuadrangle
USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 5), ans(2, 5)

! vertices of the unit quadrangle and an interior point
xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, &
               0.0_DFP, 1.0_DFP, 0.25_DFP, 0.75_DFP], [2_I4B, 5_I4B])

ans = FromUnitQuadrangle2BiUnitQuadrangle(xin)
CALL OK(ALL(ABS(ans - (2.0_DFP * xin - 1.0_DFP)) .LT. tol), &
        "ans = 2 xin - 1")
CALL OK(ALL(ABS(ans(:, 5) - [-0.5_DFP, 0.5_DFP]) .LT. tol), &
        "interior point")

! inverse of FromBiUnitQuadrangle2UnitQuadrangle
ans = FromBiUnitQuadrangle2UnitQuadrangle( &
      FromUnitQuadrangle2BiUnitQuadrangle(xin))
CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")

END PROGRAM main
