!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgInsertionSort for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgInsertionSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(8), a0(8), arg(8)
INTEGER(I4B) :: ii

a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
a0 = a

! arg must contain 1..n on entry
arg = [(ii, ii=1, 8)]
CALL ArgInsertionSort(a, arg, 1_I4B, 8_I4B)
! the sort is stable: the two 1s keep their order (3 before 5)
CALL OK(ALL(arg .EQ. [3_I4B, 5_I4B, 8_I4B, 7_I4B, 1_I4B, 4_I4B, &
                      6_I4B, 2_I4B]), "arg")
CALL OK(ALL(a(arg(1:7)) .LE. a(arg(2:8))), "a(arg) is sorted")
CALL OK(ALL(a .EQ. a0), "array is not changed")

! only arg(5:8) is sorted, arg(1:4) is not changed
arg = [(ii, ii=1, 8)]
CALL ArgInsertionSort(a, arg, 5_I4B, 8_I4B)
CALL OK(ALL(arg .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 8_I4B, &
                      7_I4B, 6_I4B]), "sub-range arg(5:8)")

END PROGRAM main
