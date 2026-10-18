!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Size(n, d, upto), number of multi-indices of order <= n
!
! Description:
!   Includes the regression test for bug H-09 in
!   bug-reports/Utility_bug_report.md: the value of upto was not
!   used, so upto = .FALSE. gave the result for .TRUE.

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: SIZE
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii, total

! d = 1: 1 + 2 + 3 + 4
CALL OK(SIZE(3_I4B, 1_I4B, .TRUE.) .EQ. 10_I4B, "Size(3, 1, .TRUE.)")

! d = 2: 1 + 3 + 6
CALL OK(SIZE(2_I4B, 2_I4B, upto=.TRUE.) .EQ. 10_I4B, &
        "Size(2, 2, .TRUE.)")

! order zero
CALL OK(SIZE(0_I4B, 2_I4B, .TRUE.) .EQ. 1_I4B, "Size(0, 2, .TRUE.)")

! the result is the sum of Size(ii, d) for ii = 0, ..., n
total = 0_I4B
DO ii = 0, 3
  total = total + SIZE(ii, 2_I4B)
END DO
CALL OK(SIZE(3_I4B, 2_I4B, .TRUE.) .EQ. total, "sum of Size(ii, d)")
CALL OK(total .EQ. 20_I4B, "Size(3, 2, .TRUE.) is 20")

! upto = .FALSE. counts only the tuples of order n
CALL OK(SIZE(3_I4B, 1_I4B, .FALSE.) .EQ. SIZE(3_I4B, 1_I4B), &
        "Size(3, 1, .FALSE.) = Size(3, 1)")
CALL OK(SIZE(2_I4B, 2_I4B, upto=.FALSE.) .EQ. 6_I4B, &
        "Size(2, 2, .FALSE.) is 6")

END PROGRAM main
