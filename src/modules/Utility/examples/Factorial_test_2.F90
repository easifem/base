!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Factorial of a negative integer
!
! Description:
!   Regression test for bug M-33 in bug-reports/Utility_bug_report.md.
!   The recursion never stopped for a negative N. Now the result is 0.

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Factorial
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(Factorial(-1_I4B) .EQ. 0_I4B, "(-1)! gives 0")
CALL OK(Factorial(-10_I4B) .EQ. 0_I4B, "(-10)! gives 0")
CALL OK(Factorial(2_I4B) .EQ. 2_I4B, "2! = 2")

END PROGRAM main
