!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Radian converts degrees to radians

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Radian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: piTol = 1.0E-8_DFP
REAL(DFP) :: pi

pi = ACOS(-1.0_DFP)

! real argument
CALL OK(ABS(Radian(0.0_DFP)) .LT. tol, "Radian(0)")
CALL OK(ABS(Radian(180.0_DFP) - 3.1415926535_DFP) .LT. tol, &
        "Radian(180) uses pi = 3.1415926535")
CALL OK(ABS(Radian(180.0_DFP) - pi) .LT. piTol, "Radian(180) ~ pi")
CALL OK(ABS(Radian(90.0_DFP) - 0.5_DFP * pi) .LT. piTol, &
        "Radian(90) ~ pi/2")
CALL OK(ABS(Radian(-45.0_DFP) + 0.25_DFP * pi) .LT. piTol, &
        "Radian(-45) ~ -pi/4")

! integer argument gives the same value as the real one
CALL OK(ABS(Radian(90_I4B) - Radian(90.0_DFP)) .LT. tol, &
        "Radian(90) integer = real")
CALL OK(ABS(Radian(360_I4B) - 2.0_DFP * pi) .LT. piTol, &
        "Radian(360) ~ 2 pi")

END PROGRAM main
