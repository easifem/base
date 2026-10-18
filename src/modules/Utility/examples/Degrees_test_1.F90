!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Degrees converts radians to degrees

PROGRAM main
USE GlobalData, ONLY: DFP
USE MiscUtility, ONLY: Degrees
USE MiscUtility, ONLY: Radian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: piTol = 1.0E-6_DFP
REAL(DFP) :: pi

pi = ACOS(-1.0_DFP)

CALL OK(ABS(Degrees(0.0_DFP)) .LT. tol, "Degrees(0)")
CALL OK(ABS(Degrees(3.1415926535_DFP) - 180.0_DFP) .LT. tol, &
        "Degrees(3.1415926535) = 180")
CALL OK(ABS(Degrees(pi) - 180.0_DFP) .LT. piTol, "Degrees(pi) ~ 180")
CALL OK(ABS(Degrees(-0.5_DFP * pi) + 90.0_DFP) .LT. piTol, &
        "Degrees(-pi/2) ~ -90")

! Degrees is the inverse of Radian
CALL OK(ABS(Degrees(Radian(37.5_DFP)) - 37.5_DFP) .LT. tol, &
        "Degrees(Radian(x)) = x")

END PROGRAM main
