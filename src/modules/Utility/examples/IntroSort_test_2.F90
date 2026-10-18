!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IntroSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: IntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(7), b(40)
INTEGER(I4B) :: ii

! short vector with negative entries
a = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
CALL IntroSort(a)
CALL OK(ALL(a .EQ. [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, 2.25_DFP, &
                    3.5_DFP, 7.75_DFP]), "short vector")

! long vector, a permutation of 0.5 * (1..40)
b = [(0.5_DFP * MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
CALL IntroSort(b)
CALL OK(ALL(b .EQ. [(0.5_DFP * ii, ii=1, 40)]), "long vector")
CALL OK(ALL(b(1:39) .LE. b(2:40)), "ascending order")

! reverse sorted with duplicates
b = [(REAL(MOD(41_I4B - ii, 5_I4B), DFP), ii=1, 40)]
CALL IntroSort(b)
CALL OK(ALL(b(1:39) .LE. b(2:40)), "duplicates are sorted")
CALL OK(COUNT(b .EQ. 0.0_DFP) .EQ. 8_I4B, "duplicates are kept")

END PROGRAM main
