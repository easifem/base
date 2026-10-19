!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Repeat for a real vector
!
! Description:
!   Includes the regression test for bug M-14 in
!   bug-reports/Utility_bug_report.md: rtimes <= 0 wrote val into
!   the empty result.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Repeat
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: val(2), ans(6)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

val = [1.5_DFP, -2.5_DFP]

ans = Repeat(val, 3_I4B)
CALL OK(ALL(ABS(ans - [val, val, val]) .LT. tol), "repeat 3 times")
CALL OK(ABS(ans(5) - 1.5_DFP) .LT. tol, "fifth entry")
CALL OK(SIZE(Repeat(val, 4_I4B)) .EQ. 8_I4B, "size of the result")

! rtimes <= 0 gives an empty vector
CALL OK(SIZE(Repeat(val, 0_I4B)) .EQ. 0_I4B, "rtimes = 0")

END PROGRAM main
