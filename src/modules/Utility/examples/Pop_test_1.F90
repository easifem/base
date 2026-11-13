!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Pop for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE PushPopUtility, ONLY: Pop
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: vec(5), one(1)
INTEGER(I4B), ALLOCATABLE :: ans(:)

vec = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]

! pop the first entry
ans = Pop(vec, 1_I4B)
CALL OK(ALL(ans .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), "pop at pos=1")

! pop a middle entry
ans = Pop(vec, 2_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 3_I4B, 4_I4B, 5_I4B]), "pop at pos=2")

! pop the last entry
ans = Pop(vec, 5_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "pop at pos=5")

! out of range positions
ans = Pop(vec, 100_I4B)
CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), &
        "pos > SIZE(vec) pops the last entry")
ans = Pop(vec, 0_I4B)
CALL OK(ALL(ans .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "pos < 1 pops the first entry")

! a vector of size 1 gives an empty vector
one = [7_I4B]
ans = Pop(one, 1_I4B)
CALL OK(SIZE(ans) .EQ. 0_I4B, "pop from a vector of size 1")

END PROGRAM main
