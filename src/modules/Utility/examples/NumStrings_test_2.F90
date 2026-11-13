!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: NumStrings with a repeat count at the start
!
! Description:
!   Regression test for bug M-29 in bug-reports/Utility_bug_report.md.
!   A repeat count at the start of the string was expanded only when
!   the string started with a blank.

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: NumStrings
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(NumStrings("3*1.0 2.0") .EQ. 4_I4B, "3*1.0 2.0")
CALL OK(NumStrings(" 3*1.0 2.0") .EQ. 4_I4B, "leading blank")
CALL OK(NumStrings("2*a") .EQ. 2_I4B, "only a repeat count")
CALL OK(NumStrings("abc") .EQ. 1_I4B, "one word")
CALL OK(NumStrings("a b") .EQ. 2_I4B, "two words")

END PROGRAM main
