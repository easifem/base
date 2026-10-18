!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Push for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE PushPopUtility, ONLY: Push
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: vec(5)
INTEGER(I4B), ALLOCATABLE :: ans(:)

vec = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]

! push at the front
ans = Push(vec, 1_I4B, 0_I4B)
CALL OK(ALL(ans .EQ. [0_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "push at pos=1")

! push in the middle
ans = Push(vec, 2_I4B, 0_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 0_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "push at pos=2")

! pos = SIZE(vec) puts value before the last entry
ans = Push(vec, 5_I4B, 0_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 0_I4B, 5_I4B]), &
        "push at pos=5")

! pos = SIZE(vec) + 1 appends
ans = Push(vec, 6_I4B, 0_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 0_I4B]), &
        "push at pos=6")

! out of range positions
ans = Push(vec, 100_I4B, 0_I4B)
CALL OK(ans(6) .EQ. 0_I4B, "pos > SIZE(vec) appends")
ans = Push(vec, 0_I4B, 9_I4B)
CALL OK(ans(1) .EQ. 9_I4B .AND. ALL(ans(2:6) .EQ. vec), &
        "pos < 1 prepends")

! the result has one more entry
CALL OK(SIZE(Push(vec, 3_I4B, 0_I4B)) .EQ. 6_I4B, "size of the result")

END PROGRAM main
