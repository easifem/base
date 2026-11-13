!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: QuickSort for one real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: QuickSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(7), b(40)
INTEGER(I4B) :: ii

a = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
CALL QuickSort(a, 1_I4B, 7_I4B)
CALL OK(ALL(a .EQ. [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, 2.25_DFP, &
                    3.5_DFP, 7.75_DFP]), "whole vector")

! already sorted and with duplicates
b = [(REAL(MOD(7_I4B * ii, 5_I4B), DFP), ii=1, 40)]
CALL QuickSort(b, 1_I4B, 40_I4B)
CALL OK(ALL(b(1:39) .LE. b(2:40)), "duplicates")
CALL QuickSort(b, 1_I4B, 40_I4B)
CALL OK(ALL(b(1:39) .LE. b(2:40)), "already sorted")

END PROGRAM main
