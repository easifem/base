!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: NumMatchStr counts the occurrences of a pattern

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: NumMatchStr
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=10) :: astr

CALL OK(NumMatchStr("hello world", "o") .EQ. 2_I4B, "two matches")
CALL OK(NumMatchStr("hello world", "xyz") .EQ. 0_I4B, "no match")
CALL OK(NumMatchStr("ab", "abc") .EQ. 0_I4B, "pattern too long")

! overlapping matches are counted
CALL OK(NumMatchStr("aaaa", "aa") .EQ. 3_I4B, "overlapping matches")

! trailing blanks of the string are searched too
astr = "a b"
CALL OK(NumMatchStr(astr, " ") .EQ. 8_I4B, "padded string")
CALL OK(NumMatchStr(TRIM(astr), " ") .EQ. 1_I4B, "trimmed string")

END PROGRAM main
