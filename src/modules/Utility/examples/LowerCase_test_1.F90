!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: LowerCase returns a lower case copy of a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: LowerCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: astr = "Hello World 123!"
CHARACTER(:), ALLOCATABLE :: ans

ans = LowerCase(astr)
CALL OK(ans .EQ. "hello world 123!", "mixed case")
CALL OK(LEN(ans) .EQ. LEN(astr), "length is kept")

CALL OK(LowerCase("ABC_XYZ") .EQ. "abc_xyz", "only A-Z change")
CALL OK(LowerCase("abc") .EQ. "abc", "already lower case")

ans = LowerCase("")
CALL OK(LEN(ans) .EQ. 0_I4B, "empty string")

END PROGRAM main
