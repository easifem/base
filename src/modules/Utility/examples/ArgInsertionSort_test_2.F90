!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgInsertionSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgInsertionSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(6)
INTEGER(I4B) :: arg(6)
INTEGER(I4B) :: ii

a = [0.5_DFP, -2.0_DFP, 3.25_DFP, -2.0_DFP, 1.0_DFP, 0.0_DFP]
arg = [(ii, ii=1, 6)]
CALL ArgInsertionSort(a, arg, 1_I4B, 6_I4B)
CALL OK(ALL(arg .EQ. [2_I4B, 4_I4B, 6_I4B, 1_I4B, 5_I4B, 3_I4B]), "arg")
CALL OK(ALL(a(arg) .EQ. [-2.0_DFP, -2.0_DFP, 0.0_DFP, 0.5_DFP, &
                         1.0_DFP, 3.25_DFP]), "a(arg) is sorted")

! arg does not have to start as 1..n: only arg(low:high) is reordered
arg = [6_I4B, 5_I4B, 4_I4B, 3_I4B, 2_I4B, 1_I4B]
CALL ArgInsertionSort(a, arg, 1_I4B, 6_I4B)
CALL OK(ALL(a(arg(1:5)) .LE. a(arg(2:6))), "any permutation as input")

END PROGRAM main
