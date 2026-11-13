!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Safe_ACOS clamps its argument to [-1, 1]

PROGRAM main
USE GlobalData, ONLY: DFP
USE MiscUtility, ONLY: Safe_ACOS
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: pi, c(3)

pi = ACOS(-1.0_DFP)

CALL OK(ABS(Safe_ACOS(0.5_DFP) - ACOS(0.5_DFP)) .LT. tol, &
        "inside [-1,1] same as ACOS")

! round-off just outside [-1, 1] is clamped
CALL OK(ABS(Safe_ACOS(1.0_DFP + 1.0E-12_DFP)) .LT. tol, &
        "slightly above 1 gives 0")
CALL OK(ABS(Safe_ACOS(-1.0_DFP - 1.0E-12_DFP) - pi) .LT. tol, &
        "slightly below -1 gives pi")
CALL OK(ABS(Safe_ACOS(5.0_DFP)) .LT. tol, "any c > 1 gives 0")

! Safe_ACOS is elemental
c = [-2.0_DFP, 0.0_DFP, 2.0_DFP]
CALL OK(ALL(ABS(Safe_ACOS(c) - [pi, 0.5_DFP * pi, 0.0_DFP]) .LT. tol), &
        "elemental")

END PROGRAM main
