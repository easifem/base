!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get one entry of an integer vector
!
! Description:
!   Includes the regression test for bug M-15 in
!   bug-reports/Utility_bug_report.md: indx <= 0 read val out of
!   bounds; it now gives zero, like indx > SIZE(val).

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Get
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: val(3)

val = [10_I4B, 20_I4B, 30_I4B]

CALL OK(Get(val, 1_I4B) .EQ. 10_I4B, "first entry")
CALL OK(Get(val=val, indx=3_I4B) .EQ. 30_I4B, "last entry")

! indx outside 1..SIZE(val) returns zero
CALL OK(Get(val, 4_I4B) .EQ. 0_I4B, "indx out of range")
CALL OK(Get(val, 0_I4B) .EQ. 0_I4B, "indx = 0")
CALL OK(Get(val, -1_I4B) .EQ. 0_I4B, "indx < 0")

END PROGRAM main
