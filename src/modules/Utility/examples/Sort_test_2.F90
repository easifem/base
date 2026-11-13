!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Sort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: Sort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: x(7), y(7), b(40)
INTEGER(I4B) :: ii

x = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
y = [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, 2.25_DFP, 3.5_DFP, 7.75_DFP]

CALL OK(ALL(Sort(x) .EQ. y), "default")
CALL OK(ALL(Sort(x, "quicksort") .EQ. y), "quicksort")
CALL OK(ALL(Sort(x, "heapsort") .EQ. y), "heapsort")
CALL OK(ALL(Sort(x, "introsort") .EQ. y), "introsort")
CALL OK(ALL(Sort(x, "insertionsort") .EQ. y), "insertionsort")

! duplicates
b = [(REAL(MOD(7_I4B * ii, 5_I4B), DFP), ii=1, 40)]
b = Sort(b)
CALL OK(ALL(b(1:39) .LE. b(2:40)), "duplicates")

END PROGRAM main
