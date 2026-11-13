!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ToUpperCase converts a string to upper case in place

PROGRAM main
USE StringUtility, ONLY: ToUpperCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=20) :: astr

astr = "Hello, World"
CALL ToUpperCase(astr)
CALL OK(astr .EQ. "HELLO, WORLD", "mixed case")

astr = "a1_b2"
CALL ToUpperCase(astr)
CALL OK(astr .EQ. "A1_B2", "digits and _ are unchanged")

astr = "ABC"
CALL ToUpperCase(astr)
CALL OK(astr .EQ. "ABC", "already upper case")

END PROGRAM main
