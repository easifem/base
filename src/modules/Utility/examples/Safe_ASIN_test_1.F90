!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Safe_ASIN clamps its argument to [-1, 1]

PROGRAM main
USE GlobalData, ONLY: DFP
USE MiscUtility, ONLY: Safe_ASIN
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: pi, s(3)

pi = ACOS(-1.0_DFP)

CALL OK(ABS(Safe_ASIN(0.5_DFP) - ASIN(0.5_DFP)) .LT. tol, &
        "inside [-1,1] same as ASIN")

! round-off just outside [-1, 1] is clamped
CALL OK(ABS(Safe_ASIN(1.0_DFP + 1.0E-12_DFP) - 0.5_DFP * pi) .LT. tol, &
        "slightly above 1 gives pi/2")
CALL OK(ABS(Safe_ASIN(-1.0_DFP - 1.0E-12_DFP) + 0.5_DFP * pi) .LT. tol, &
        "slightly below -1 gives -pi/2")

! Safe_ASIN is elemental
s = [-3.0_DFP, 0.0_DFP, 3.0_DFP]
CALL OK(ALL(ABS(Safe_ASIN(s) - [-0.5_DFP * pi, 0.0_DFP, 0.5_DFP * pi]) &
            .LT. tol), "elemental")

END PROGRAM main
