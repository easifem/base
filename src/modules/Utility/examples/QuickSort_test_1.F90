!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: QuickSort for one integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: QuickSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(8), b(40), e(0)
INTEGER(I4B) :: ii

! the whole vector
a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
CALL QuickSort(a, 1_I4B, 8_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 6_I4B, 7_I4B, &
                    8_I4B]), "whole vector")

! only a(3:6) is sorted, the other entries are not changed
a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
CALL QuickSort(a, 3_I4B, 6_I4B)
CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 1_I4B, 6_I4B, 7_I4B, 9_I4B, 3_I4B, &
                    2_I4B]), "sub-range a(3:6)")

! a permutation of 1..40
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
CALL QuickSort(b, 1_I4B, 40_I4B)
CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")

! empty vector (high < low)
CALL QuickSort(e, 1_I4B, 0_I4B)
CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")

END PROGRAM main
