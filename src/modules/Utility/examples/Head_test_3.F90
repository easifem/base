!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HEAD of a string, the first character

PROGRAM main
USE GlobalData, ONLY: I4B
USE HeadUtility, ONLY: HEAD
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=16) :: aline

CALL OK(HEAD("Hello") .EQ. "H", "head of Hello")
CALL OK(HEAD("a") .EQ. "a", "head of a string of length 1")
CALL OK(LEN(HEAD("abc")) .EQ. 1_I4B, "result has length 1")

! a leading blank is the head
CALL OK(HEAD(" x") .EQ. " ", "head is a leading blank")

! a variable with a trailing blank padding
aline = "World"
CALL OK(HEAD(aline) .EQ. "W", "head of a padded variable")
CALL OK(HEAD(TRIM(aline)) .EQ. "W", "head of TRIM(aline)")

! case is preserved
CALL OK(HEAD("hello") .NE. HEAD("Hello"), "case matters")

END PROGRAM main
