!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Split for strings

PROGRAM main
USE GlobalData, ONLY: I4B
USE SplitUtility, ONLY: Split
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(:), ALLOCATABLE :: s

! even length
CALL OK(Split("abcdef", 1) .EQ. "abc", "even length, section 1")
CALL OK(Split("abcdef", 2) .EQ. "def", "even length, section 2")

! odd length: the middle character goes to the second half
s = Split("hello", 1)
CALL OK(s .EQ. "he", "odd length, section 1")
CALL OK(LEN(s) .EQ. 2_I4B, "odd length, length of section 1")
s = Split("hello", 2)
CALL OK(s .EQ. "llo", "odd length, section 2")
CALL OK(LEN(s) .EQ. 3_I4B, "odd length, length of section 2")

! length 1
s = Split("x", 1)
CALL OK(LEN(s) .EQ. 0_I4B, "length 1, section 1 is empty")
s = Split("x", 2)
CALL OK(s .EQ. "x", "length 1, section 2")

! any other section gives an empty string
s = Split("abcdef", 3)
CALL OK(LEN(s) .EQ. 0_I4B, "section 3 gives an empty string")
s = Split("abcdef", 0)
CALL OK(LEN(s) .EQ. 0_I4B, "section 0 gives an empty string")

! trailing blanks are part of the string
s = Split("ab  ", 2)
CALL OK(LEN(s) .EQ. 2_I4B, "trailing blanks are kept")

END PROGRAM main
