!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: InsertionSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: InsertionSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(6)

! the whole vector
a = [0.5_DFP, -2.0_DFP, 3.25_DFP, -2.0_DFP, 1.0_DFP, 0.0_DFP]
CALL InsertionSort(a, 1_I4B, 6_I4B)
CALL OK(ALL(a .EQ. [-2.0_DFP, -2.0_DFP, 0.0_DFP, 0.5_DFP, 1.0_DFP, &
                    3.25_DFP]), "whole vector")

! only a(1:3) is sorted
a = [0.5_DFP, -2.0_DFP, 3.25_DFP, -2.0_DFP, 1.0_DFP, 0.0_DFP]
CALL InsertionSort(a, 1_I4B, 3_I4B)
CALL OK(ALL(a .EQ. [-2.0_DFP, 0.5_DFP, 3.25_DFP, -2.0_DFP, 1.0_DFP, &
                    0.0_DFP]), "sub-range a(1:3)")

! already sorted vector is not changed
a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL InsertionSort(a, 1_I4B, 6_I4B)
CALL OK(ALL(a .EQ. [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                    6.0_DFP]), "already sorted")

END PROGRAM main
