!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: InsertionSort for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: InsertionSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(8), e(0)

! the whole vector
a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
CALL InsertionSort(a, 1_I4B, 8_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 6_I4B, 7_I4B, &
                    8_I4B]), "whole vector")

! only a(3:6) is sorted, the other entries are not changed
a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
CALL InsertionSort(a, 3_I4B, 6_I4B)
CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 1_I4B, 6_I4B, 7_I4B, 9_I4B, 3_I4B, &
                    2_I4B]), "sub-range a(3:6)")

! low = high and low > high do nothing
a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
CALL InsertionSort(a, 5_I4B, 5_I4B)
CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, &
                    2_I4B]), "low = high")
CALL InsertionSort(a, 6_I4B, 2_I4B)
CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, &
                    2_I4B]), "low > high")

! empty vector
CALL InsertionSort(e, 1_I4B, 0_I4B)
CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")

END PROGRAM main
