!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitLine2BiUnitLine_ maps [0, 1] to [-1, 1], no allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitLine2BiUnitLine_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), ans(5)
INTEGER(I4B) :: tsize

xin = [0.0_DFP, 0.5_DFP, 1.0_DFP]

ans = -100.0_DFP
CALL FromUnitLine2BiUnitLine_(xin, ans, tsize)
CALL OK(tsize .EQ. 3_I4B, "tsize is SIZE(xin)")
CALL OK(ALL(ABS(ans(1:tsize) - [-1.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
        "mapped values")
CALL OK(ALL(ABS(ans(4:5) + 100.0_DFP) .LT. tol), &
        "extra entries are not changed")

END PROGRAM main
