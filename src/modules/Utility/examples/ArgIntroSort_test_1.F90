!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgIntroSort for an integer vector
!
! Description:
!   Includes the regression test for bug M-08 in
!   bug-reports/Utility_bug_report.md: an empty vector evaluated
!   LOG(0.0); the routine now returns at once for 0 or 1 entries.

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgIntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(10), a0(10), arg(10)
INTEGER(I4B) :: b(40), b0(40), argb(40)
INTEGER(I4B) :: e(0), arge(0), c(1), argc(1)
INTEGER(I4B) :: ii

! arg must contain 1..n on entry
a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
a0 = a
arg = [(ii, ii=1, 10)]
CALL ArgIntroSort(a, arg)
CALL OK(ALL(arg .EQ. [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, &
                      5_I4B, 7_I4B, 3_I4B, 9_I4B]), "arg")
CALL OK(ALL(a(arg) .EQ. [(ii, ii=1, 10)]), "a(arg) is sorted")
CALL OK(ALL(a .EQ. a0), "array is not changed")

! long vector, a permutation of 1..40
b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
b0 = b
argb = [(ii, ii=1, 40)]
CALL ArgIntroSort(b, argb)
CALL OK(ALL(b(argb) .EQ. [(ii, ii=1, 40)]), "long vector")
CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
        "arg is a permutation")
CALL OK(ALL(b .EQ. b0), "long array is not changed")

! duplicate entries
b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
argb = [(ii, ii=1, 40)]
CALL ArgIntroSort(b, argb)
CALL OK(ALL(b(argb(1:39)) .LE. b(argb(2:40))), "duplicates")
CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
        "arg is a permutation with duplicates")

! vector of size 1 and empty vector
c = [42_I4B]
argc = [1_I4B]
CALL ArgIntroSort(c, argc)
CALL OK(argc(1) .EQ. 1_I4B, "size 1")
CALL ArgIntroSort(e, arge)
CALL OK(SIZE(arge) .EQ. 0_I4B, "empty vector")

END PROGRAM main
