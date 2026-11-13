!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HEAD of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE HeadUtility, ONLY: HEAD
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(4), y(1), z(3)

x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
CALL OK(HEAD(x) .EQ. 10_I4B, "head of a vector")
CALL OK(HEAD(x) .EQ. x(1), "head is x(1)")
CALL OK(HEAD(x) .NE. x(SIZE(x)), "head is not the last element")

! array section
CALL OK(HEAD(x(2:3)) .EQ. 20_I4B, "head of x(2:3)")

! vector with a single element
y = [7_I4B]
CALL OK(HEAD(y) .EQ. 7_I4B, "head of a vector of size 1")

! negative values
z = [-5_I4B, 0_I4B, 5_I4B]
CALL OK(HEAD(z) .EQ. -5_I4B, "head of a vector with negative entry")

END PROGRAM main
