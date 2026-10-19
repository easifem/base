!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FindReplace reports a result that does not fit
!
! Description:
!   Regression test for bug M-28 in bug-reports/Utility_bug_report.md.
!   If the result did not fit in LEN(chars), chars was left unchanged
!   without any sign of it. The optional isok is now .FALSE. then.

PROGRAM main
USE GlobalData, ONLY: LGT
USE StringUtility, ONLY: FindReplace
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=5) :: cstr
LOGICAL(LGT) :: isok

cstr = "abcde"
CALL FindReplace(cstr, "a", "aa", isok)
CALL OK(.NOT. isok, "no room: isok is false")
CALL OK(cstr .EQ. "abcde", "no room: string is unchanged")

cstr = "abc"
CALL FindReplace(cstr, "a", "aa", isok)
CALL OK(isok, "fits: isok is true")
CALL OK(cstr .EQ. "aabc", "fits: string is changed")

cstr = "abc"
CALL FindReplace(cstr, "x", "y", isok)
CALL OK(isok, "no match: isok is true")

END PROGRAM main
