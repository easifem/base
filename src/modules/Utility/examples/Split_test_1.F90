!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Split for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE SplitUtility, ONLY: Split
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: even(6), odd(5), one(1)
INTEGER(I4B), ALLOCATABLE :: b(:)

even = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
odd = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]
one = [7_I4B]

! even size: two halves of equal size
b = Split(even, 1)
CALL OK(ALL(b .EQ. [1_I4B, 2_I4B, 3_I4B]), "even size, section 1")
b = Split(even, 2)
CALL OK(ALL(b .EQ. [4_I4B, 5_I4B, 6_I4B]), "even size, section 2")

! odd size: the middle entry goes to the second half
b = Split(odd, 1)
CALL OK(SIZE(b) .EQ. 2_I4B, "odd size, size of section 1")
CALL OK(ALL(b .EQ. [1_I4B, 2_I4B]), "odd size, section 1")
b = Split(odd, 2)
CALL OK(SIZE(b) .EQ. 3_I4B, "odd size, size of section 2")
CALL OK(ALL(b .EQ. [3_I4B, 4_I4B, 5_I4B]), "odd size, section 2")

! size 1: section 1 is empty, section 2 is x(1)
b = Split(one, 1)
CALL OK(SIZE(b) .EQ. 0_I4B, "size 1, section 1 is empty")
b = Split(one, 2)
CALL OK(SIZE(b) .EQ. 1_I4B, "size 1, size of section 2")
CALL OK(b(1) .EQ. 7_I4B, "size 1, section 2 is x(1)")

! the two halves give back the input
CALL OK(ALL([Split(odd, 1), Split(odd, 2)] .EQ. odd), &
        "halves joined give the input")

END PROGRAM main
