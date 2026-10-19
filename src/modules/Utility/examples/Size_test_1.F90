!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Size(n, d), number of multi-indices of order n
!
! Description:
!   Includes the regression test for bug M-16 in
!   bug-reports/Utility_bug_report.md: d = 0 with n >= 1 recursed
!   with negative d; the only tuple of one entry is [n].

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: SIZE
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(5)

! d = 1: tuples (i, n-i), i = 0, ..., n
CALL OK(SIZE(3_I4B, 1_I4B) .EQ. 4_I4B, "Size(3, 1)")

! d = 2: binomial coefficient (n+2 choose 2)
CALL OK(SIZE(2_I4B, 2_I4B) .EQ. 6_I4B, "Size(2, 2)")
CALL OK(SIZE(n=3_I4B, d=2_I4B) .EQ. 10_I4B, "Size(3, 2)")

! d = 3
CALL OK(SIZE(2_I4B, 3_I4B) .EQ. 10_I4B, "Size(2, 3)")

! order zero has only one tuple
CALL OK(SIZE(0_I4B, 2_I4B) .EQ. 1_I4B, "Size(0, 2)")

! d = 0: the only tuple of one entry is [n]
CALL OK(SIZE(3_I4B, 0_I4B) .EQ. 1_I4B, "Size(3, 0)")
CALL OK(SIZE(3_I4B, 0_I4B, .TRUE.) .EQ. 4_I4B, "Size(3, 0, .TRUE.)")

! the intrinsic SIZE is still available
x = 0_I4B
CALL OK(SIZE(x) .EQ. 5_I4B, "intrinsic SIZE")

END PROGRAM main
