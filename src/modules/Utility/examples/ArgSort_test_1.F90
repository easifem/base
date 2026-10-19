!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgSort for an integer vector with each algorithm name

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(10), arg(10), id(10), b(40), argb(40)
INTEGER(I4B) :: ii

x = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
arg = [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, 5_I4B, 7_I4B, &
       3_I4B, 9_I4B]
id = [(ii, ii=1, 10)]

! the default algorithm is INTROSORT
CALL OK(ALL(ArgSort(x) .EQ. arg), "default")
CALL OK(ALL(x(ArgSort(x)) .EQ. id), "x(ArgSort(x)) is sorted")
CALL OK(ALL(ArgSort(x, "INTROSORT") .EQ. arg), "INTROSORT")
CALL OK(ALL(ArgSort(x, "QUICKSORT") .EQ. arg), "QUICKSORT")
CALL OK(ALL(ArgSort(x, "HEAPSORT") .EQ. arg), "HEAPSORT")
CALL OK(ALL(ArgSort(x, "INSERTION") .EQ. arg), "INSERTION")

! the name is not case sensitive
CALL OK(ALL(ArgSort(x, "heapsort") .EQ. arg), "heapsort")

! only the first four letters are used
CALL OK(ALL(ArgSort(x, "INSERTIONSORT") .EQ. arg), "INSERTIONSORT")
CALL OK(ALL(ArgSort(x, "Quick") .EQ. arg), "Quick")

! an unknown name falls back to the default, INTROSORT
CALL OK(ALL(ArgSort(x, "BUBBLESORT") .EQ. arg), "unknown name")

! a permutation of 1..40
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
argb = ArgSort(b)
CALL OK(ALL(b(argb) .EQ. [(ii, ii=1, 40)]), "long vector")

END PROGRAM main
