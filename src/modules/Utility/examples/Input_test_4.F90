!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3), b(3), c(3)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [4.5_DFP, 5.5_DFP, 6.5_DFP]

! option is present
c = Input(a, b)
CALL OK(ALL(ABS(c - b) .LT. tol), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(ABS(c - a) .LT. tol), "option is absent")

! the size of the result is the size of default
CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")

! array sections
CALL OK(ALL(ABS(Input(a(1:2), b(2:3)) - [5.5_DFP, 6.5_DFP]) .LT. tol), &
        "option is an array section")
CALL OK(ALL(ABS(Input(a(2:3)) - [2.0_DFP, 3.0_DFP]) .LT. tol), &
        "default is an array section")

END PROGRAM main
