!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HeapSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: HeapSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(7), b(40)
INTEGER(I4B) :: ii

a = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
CALL HeapSort(a)
CALL OK(ALL(a .EQ. [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, 2.25_DFP, &
                    3.5_DFP, 7.75_DFP]), "short vector")

! reverse sorted
b = [(0.25_DFP * (41_I4B - ii), ii=1, 40)]
CALL HeapSort(b)
CALL OK(ALL(b .EQ. [(0.25_DFP * ii, ii=1, 40)]), "reverse sorted")

END PROGRAM main
