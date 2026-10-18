!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IntroSort for an integer vector
!
! Description:
!   Includes the regression test for bug M-08 in
!   bug-reports/Utility_bug_report.md: an empty vector evaluated
!   LOG(0.0); the routine now returns at once for 0 or 1 entries.

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: IntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(10), b(40), c(1), d(8), e(0)
INTEGER(I4B) :: ii

! short vector (insertion sort is used)
a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
CALL IntroSort(a)
CALL OK(ALL(a .EQ. [(ii, ii=1, 10)]), "short vector")

! long vector, a permutation of 1..40 (partitioning is used)
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
CALL IntroSort(b)
CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")

! already sorted and reverse sorted
b = [(ii, ii=1, 40)]
CALL IntroSort(b)
CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "already sorted")
b = [(41_I4B - ii, ii=1, 40)]
CALL IntroSort(b)
CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "reverse sorted")

! duplicate entries
d = [3_I4B, 1_I4B, 3_I4B, 2_I4B, 1_I4B, 3_I4B, 2_I4B, 1_I4B]
CALL IntroSort(d)
CALL OK(ALL(d .EQ. [1_I4B, 1_I4B, 1_I4B, 2_I4B, 2_I4B, 3_I4B, &
                    3_I4B, 3_I4B]), "duplicates")

! vector of size 1
c = [42_I4B]
CALL IntroSort(c)
CALL OK(c(1) .EQ. 42_I4B, "size 1")

! empty vector
CALL IntroSort(e)
CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")

END PROGRAM main
