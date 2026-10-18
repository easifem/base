!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HeapSort for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: HeapSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(10), b(40), c(1), e(0)
INTEGER(I4B) :: ii

a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
CALL HeapSort(a)
CALL OK(ALL(a .EQ. [(ii, ii=1, 10)]), "short vector")

! a permutation of 1..40
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
CALL HeapSort(b)
CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")

! duplicates
b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
CALL HeapSort(b)
CALL OK(ALL(b(1:39) .LE. b(2:40)), "duplicates")
CALL OK(COUNT(b .EQ. 4_I4B) .EQ. 8_I4B, "duplicates are kept")

! size 1 and size 0
c = [42_I4B]
CALL HeapSort(c)
CALL OK(c(1) .EQ. 42_I4B, "size 1")
CALL HeapSort(e)
CALL OK(SIZE(e) .EQ. 0_I4B, "size 0")

END PROGRAM main
