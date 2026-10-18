!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for scalar reals

PROGRAM main
USE GlobalData, ONLY: DFP
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a, b
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = 1.5_DFP
b = 2.5_DFP

! option is present
CALL OK(ABS(Input(a, b) - 2.5_DFP) .LT. tol, "option is present")
CALL OK(ABS(Input(default=a, option=b) - b) .LT. tol, "keyword arguments")

! option is absent
CALL OK(ABS(Input(a) - 1.5_DFP) .LT. tol, "option is absent")
CALL OK(ABS(Input(default=0.25_DFP) - 0.25_DFP) .LT. tol, "literal default")

! option is present and equal to zero
CALL OK(ABS(Input(a, 0.0_DFP)) .LT. tol, "option is zero")

! negative values
CALL OK(ABS(Input(-3.0_DFP, -7.5_DFP) + 7.5_DFP) .LT. tol, &
        "negative option")

END PROGRAM main
