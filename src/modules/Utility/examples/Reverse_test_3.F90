!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reverse a block of an integer matrix along a dimension

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReverseUtility, ONLY: Reverse
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(4, 4), a0(4, 4), b(4, 4)
INTEGER(I4B) :: ii

a0 = RESHAPE([(ii, ii=1, 16)], [4, 4])

! dim=1: reverse the order of rows 2:4 in columns 2:3
a = a0
CALL Reverse(a, 2_I4B, 4_I4B, 2_I4B, 3_I4B, 1_I4B)
b = a0
b(2:4, 2:3) = a0(4:2:-1, 2:3)
CALL OK(ALL(a .EQ. b), "dim=1 on a(2:4, 2:3)")
CALL OK(a(1, 2) .EQ. a0(1, 2), "row 1 is untouched")
CALL OK(ALL(a(:, 1) .EQ. a0(:, 1)), "column 1 is untouched")

! dim=2: reverse the order of columns 1:4 in rows 1:3
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 4_I4B, 2_I4B)
b = a0
b(1:3, :) = a0(1:3, 4:1:-1)
CALL OK(ALL(a .EQ. b), "dim=2 on a(1:3, 1:4)")
CALL OK(ALL(a(4, :) .EQ. a0(4, :)), "row 4 is untouched")

! an invalid dim does nothing
a = a0
CALL Reverse(a, 1_I4B, 4_I4B, 1_I4B, 4_I4B, 3_I4B)
CALL OK(ALL(a .EQ. a0), "dim=3 does nothing for a matrix")

END PROGRAM main
