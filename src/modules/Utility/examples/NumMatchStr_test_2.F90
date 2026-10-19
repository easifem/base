!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: NumMatchStr and StrFind with an empty pattern
!
! Description:
!   Regression test for bug M-31 in bug-reports/Utility_bug_report.md.
!   An empty pattern matched at every position, so NumMatchStr gave
!   LEN(chars). Now an empty pattern has no match.

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: NumMatchStr
USE StringUtility, ONLY: StrFind
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: indices(:)

CALL OK(NumMatchStr("abc", "") .EQ. 0_I4B, "NumMatchStr: empty pattern")

CALL StrFind("abc", "", indices)
CALL OK(SIZE(indices) .EQ. 0_I4B, "StrFind: empty pattern")

END PROGRAM main
