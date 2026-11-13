!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Last element of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: LAST
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(4), y(1)

x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
CALL OK(LAST(x) .EQ. 40_I4B, "last of a vector")
CALL OK(LAST(x) .EQ. x(SIZE(x)), "last is x(SIZE(x))")

! array section
CALL OK(LAST(x(1:2)) .EQ. 20_I4B, "last of x(1:2)")

! vector with a single element
y = [7_I4B]
CALL OK(LAST(y) .EQ. 7_I4B, "last of a vector of size 1")

! negative values
CALL OK(LAST([1_I4B, 0_I4B, -5_I4B]) .EQ. -5_I4B, "negative last entry")

END PROGRAM main
