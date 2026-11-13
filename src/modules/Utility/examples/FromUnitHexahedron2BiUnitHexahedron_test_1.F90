!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitHexahedron2BiUnitHexahedron maps [0,1]^3 to [-1,1]^3

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromUnitHexahedron2BiUnitHexahedron
USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitHexahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 3), ans(3, 3)

xin(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xin(:, 2) = [1.0_DFP, 1.0_DFP, 1.0_DFP]
xin(:, 3) = [0.25_DFP, 0.5_DFP, 0.75_DFP]

ans = FromUnitHexahedron2BiUnitHexahedron(xin)
CALL OK(ALL(ABS(ans(:, 1) + 1.0_DFP) .LT. tol), &
        "(0,0,0) goes to (-1,-1,-1)")
CALL OK(ALL(ABS(ans(:, 2) - 1.0_DFP) .LT. tol), "(1,1,1) goes to (1,1,1)")
CALL OK(ALL(ABS(ans(:, 3) - [-0.5_DFP, 0.0_DFP, 0.5_DFP]) .LT. tol), &
        "interior point")

! inverse of FromBiUnitHexahedron2UnitHexahedron
ans = FromBiUnitHexahedron2UnitHexahedron( &
      FromUnitHexahedron2BiUnitHexahedron(xin))
CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")

END PROGRAM main
