!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for scalar integers

PROGRAM main
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a, b

a = 10_I4B
b = 20_I4B

! option is present
CALL OK(Input(a, b) .EQ. 20_I4B, "option is present")
CALL OK(Input(default=a, option=b) .EQ. b, "keyword arguments")

! option is absent
CALL OK(Input(a) .EQ. 10_I4B, "option is absent")
CALL OK(Input(default=5_I4B) .EQ. 5_I4B, "literal default")

! option is present and equal to zero
CALL OK(Input(a, 0_I4B) .EQ. 0_I4B, "option is zero")

! negative values
CALL OK(Input(-3_I4B, -7_I4B) .EQ. -7_I4B, "negative option")

END PROGRAM main
