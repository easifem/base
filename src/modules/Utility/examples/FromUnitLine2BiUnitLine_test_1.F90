!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitLine2BiUnitLine maps [0, 1] to [-1, 1]

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromUnitLine2BiUnitLine
USE MappingUtility, ONLY: FromBiUnitLine2UnitLine
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(4), ans(4)

xin = [0.0_DFP, 0.5_DFP, 1.0_DFP, 0.25_DFP]
ans = FromUnitLine2BiUnitLine(xin)
CALL OK(ALL(ABS(ans - [-1.0_DFP, 0.0_DFP, 1.0_DFP, -0.5_DFP]) .LT. tol), &
        "mapped values")

! inverse of FromBiUnitLine2UnitLine
ans = FromBiUnitLine2UnitLine(FromUnitLine2BiUnitLine(xin))
CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")

END PROGRAM main
