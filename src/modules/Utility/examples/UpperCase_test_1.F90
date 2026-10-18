!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: UpperCase returns an upper case copy of a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: UpperCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: astr = "Hello World 123!"
CHARACTER(:), ALLOCATABLE :: ans

ans = UpperCase(astr)
CALL OK(ans .EQ. "HELLO WORLD 123!", "mixed case")
CALL OK(LEN(ans) .EQ. LEN(astr), "length is kept")

CALL OK(UpperCase("abc_xyz") .EQ. "ABC_XYZ", "only a-z change")
CALL OK(UpperCase("ABC") .EQ. "ABC", "already upper case")

ans = UpperCase("")
CALL OK(LEN(ans) .EQ. 0_I4B, "empty string")

END PROGRAM main
