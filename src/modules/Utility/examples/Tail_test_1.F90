!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Tail of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: TAIL
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(4), y(1)
INTEGER(I4B), ALLOCATABLE :: z(:)

x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
CALL OK(ALL(TAIL(x) .EQ. [20_I4B, 30_I4B, 40_I4B]), "tail of a vector")
CALL OK(ALL(TAIL(x) .EQ. x(2:)), "tail is x(2:)")
CALL OK(SIZE(TAIL(x)) .EQ. SIZE(x) - 1, "size is SIZE(x) - 1")

! array section
CALL OK(ALL(TAIL(x(2:3)) .EQ. [30_I4B]), "tail of x(2:3)")

! vector with a single element gives an empty vector
y = [7_I4B]
CALL OK(SIZE(TAIL(y)) .EQ. 0_I4B, "tail of a vector of size 1")

! empty vector gives an empty vector
ALLOCATE (z(0))
CALL OK(SIZE(TAIL(z)) .EQ. 0_I4B, "tail of an empty vector")

END PROGRAM main
