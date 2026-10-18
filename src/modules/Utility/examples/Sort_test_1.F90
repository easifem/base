!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Sort for an integer vector with each algorithm name

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: Sort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(10), y(10), b(40)
INTEGER(I4B) :: ii

x = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
y = [(ii, ii=1, 10)]

! the default algorithm is INTROSORT
CALL OK(ALL(Sort(x) .EQ. y), "default")
CALL OK(ALL(Sort(x, "INTROSORT") .EQ. y), "INTROSORT")
CALL OK(ALL(Sort(x, "QUICKSORT") .EQ. y), "QUICKSORT")
CALL OK(ALL(Sort(x, "HEAPSORT") .EQ. y), "HEAPSORT")
CALL OK(ALL(Sort(x, "INSERTIONSORT") .EQ. y), "INSERTIONSORT")

! the name is not case sensitive
CALL OK(ALL(Sort(x, "HeapSort") .EQ. y), "HeapSort")

! an unknown name returns x unchanged
CALL OK(ALL(Sort(x, "BUBBLESORT") .EQ. x), "unknown name")
CALL OK(ALL(Sort(x, "INSERTION") .EQ. x), "INSERTION is unknown")

! x itself is not changed
CALL OK(x(1) .EQ. 5_I4B, "x is not changed")

! a permutation of 1..40
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
CALL OK(ALL(Sort(b) .EQ. [(ii, ii=1, 40)]), "long vector")

END PROGRAM main
