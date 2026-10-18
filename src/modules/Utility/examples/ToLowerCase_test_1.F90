!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ToLowerCase converts a string to lower case in place

PROGRAM main
USE StringUtility, ONLY: ToLowerCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=20) :: astr

astr = "Hello, World"
CALL ToLowerCase(astr)
CALL OK(astr .EQ. "hello, world", "mixed case")

astr = "A1_B2"
CALL ToLowerCase(astr)
CALL OK(astr .EQ. "a1_b2", "digits and _ are unchanged")

astr = "abc"
CALL ToLowerCase(astr)
CALL OK(astr .EQ. "abc", "already lower case")

END PROGRAM main
