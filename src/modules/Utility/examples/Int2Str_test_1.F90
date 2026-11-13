!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Int2Str converts an integer to a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Int2Str
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=15) :: s

CALL OK(TRIM(Int2Str(42_I4B)) .EQ. "42", "Int2Str(42)")
CALL OK(TRIM(Int2Str(0_I4B)) .EQ. "0", "Int2Str(0)")
CALL OK(TRIM(Int2Str(-7_I4B)) .EQ. "-7", "Int2Str(-7)")
CALL OK(TRIM(Int2Str(HUGE(0_I4B))) .EQ. "2147483647", &
        "Int2Str(HUGE)")

! the digits are left adjusted, the result has length 15
s = Int2Str(42_I4B)
CALL OK(s(1:2) .EQ. "42", "left adjusted")
CALL OK(LEN(Int2Str(42_I4B)) .EQ. 15_I4B, "length is 15")

! typical use
CALL OK("step-"//TRIM(Int2Str(3_I4B)) .EQ. "step-3", "concatenation")

END PROGRAM main
