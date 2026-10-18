!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Last character of a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: LAST
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=8) :: aline

CALL OK(LAST("hello world!") .EQ. "!", "last of hello world!")
CALL OK(LAST("a") .EQ. "a", "last of a string of length 1")
CALL OK(LEN(LAST("abc")) .EQ. 1_I4B, "result has length 1")

! a trailing blank is the last character
CALL OK(LAST("ab ") .EQ. " ", "last is a trailing blank")

! a padded variable: use TRIM to get the last non-blank character
aline = "World"
CALL OK(LAST(aline) .EQ. " ", "last of a padded variable")
CALL OK(LAST(TRIM(aline)) .EQ. "d", "last of TRIM(aline)")

END PROGRAM main
