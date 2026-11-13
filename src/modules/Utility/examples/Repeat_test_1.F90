!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Repeat for an integer vector
!
! Description:
!   Includes the regression test for bug M-14 in
!   bug-reports/Utility_bug_report.md: rtimes <= 0 wrote val into
!   the empty result.

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Repeat
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: val(3)

val = [1_I4B, 2_I4B, 3_I4B]

! repeat three times
CALL OK(SIZE(Repeat(val, 3_I4B)) .EQ. 9_I4B, "size of the result")
CALL OK(ALL(Repeat(val, 3_I4B) .EQ. [val, val, val]), "repeat 3 times")
CALL OK(ALL(Repeat(val, 2_I4B) .EQ. &
            [1_I4B, 2_I4B, 3_I4B, 1_I4B, 2_I4B, 3_I4B]), &
        "repeat 2 times")

! repeat once
CALL OK(ALL(Repeat(val=val, rtimes=1_I4B) .EQ. val), "repeat once")

! rtimes <= 0 gives an empty vector
CALL OK(SIZE(Repeat(val, 0_I4B)) .EQ. 0_I4B, "rtimes = 0")
CALL OK(SIZE(Repeat(val, -2_I4B)) .EQ. 0_I4B, "rtimes < 0")

! the intrinsic REPEAT for strings is still available
CALL OK(REPEAT("ab", 2) .EQ. "abab", "intrinsic REPEAT")

END PROGRAM main
