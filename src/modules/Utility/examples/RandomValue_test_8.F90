!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Integer RandomValue gives every value with equal probability
!
! Description:
!   Regression test for bug H-11 in bug-reports/Utility_bug_report.md.
!   The integer RandomValue(from, to) rounded a uniform real number
!   with NINT, so both end points were selected half as often as the
!   other values, and for from > to some values were never returned.
!
!   The counts of n draws are checked against n/3 with a margin of
!   1000, about 8.7 standard deviations, so a correct generator fails
!   this test with a probability below 1E-17.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 60000, expected = n / 3, margin = 1000
INTEGER(I4B) :: i, k, cnt(3)
LOGICAL(LGT) :: isok

! from < to: 1, 2 and 3 are equally likely
cnt = 0
DO i = 1, n
  k = RandomValue(from=1_I4B, to=3_I4B)
  IF (k .GE. 1_I4B .AND. k .LE. 3_I4B) cnt(k) = cnt(k) + 1
END DO
CALL OK(SUM(cnt) .EQ. n, "from < to: every value is in [1, 3]")
CALL OK(ALL(ABS(cnt - expected) .LT. margin), &
        "from < to: 1, 2 and 3 are equally likely")

! from > to: the same distribution
cnt = 0
DO i = 1, n
  k = RandomValue(from=3_I4B, to=1_I4B)
  IF (k .GE. 1_I4B .AND. k .LE. 3_I4B) cnt(k) = cnt(k) + 1
END DO
CALL OK(SUM(cnt) .EQ. n, "from > to: every value is in [1, 3]")
CALL OK(ALL(ABS(cnt - expected) .LT. margin), &
        "from > to: 1, 2 and 3 are equally likely")

! a single value
isok = .TRUE.
DO i = 1, 100
  isok = isok .AND. (RandomValue(5_I4B, 5_I4B) .EQ. 5_I4B)
END DO
CALL OK(isok, "from = to gives that value")

! selecting from a vector uses the same generator
cnt = 0
DO i = 1, n
  k = RandomValue([10_I4B, 20_I4B, 30_I4B])
  IF (k .EQ. 10_I4B) cnt(1) = cnt(1) + 1
  IF (k .EQ. 20_I4B) cnt(2) = cnt(2) + 1
  IF (k .EQ. 30_I4B) cnt(3) = cnt(3) + 1
END DO
CALL OK(SUM(cnt) .EQ. n, "vector: every value is an entry")
CALL OK(ALL(ABS(cnt - expected) .LT. margin), &
        "vector: every entry is equally likely")

END PROGRAM main
