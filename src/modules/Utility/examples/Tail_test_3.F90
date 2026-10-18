!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Tail of a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: TAIL
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=8) :: aline

CALL OK(TAIL("Hello") .EQ. "ello", "tail of Hello")
CALL OK(LEN(TAIL("Hello")) .EQ. 4_I4B, "length is LEN(x) - 1")

! a string of length 1 gives an empty string
CALL OK(LEN(TAIL("a")) .EQ. 0_I4B, "tail of a string of length 1")

! trailing blanks of a padded variable are kept
aline = "World"
CALL OK(LEN(TAIL(aline)) .EQ. 7_I4B, "tail keeps trailing blanks")
CALL OK(TAIL(TRIM(aline)) .EQ. "orld", "tail of TRIM(aline)")

END PROGRAM main
