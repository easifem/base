!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for strings

PROGRAM main
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(:), ALLOCATABLE :: s

! option is present
s = Input("abc", "xyz")
CALL OK(s .EQ. "xyz", "option is present")

! option is absent
s = Input("abc")
CALL OK(s .EQ. "abc", "option is absent")
CALL OK(LEN(s) .EQ. 3_I4B, "length of default")

! the result has the length of the selected string
s = Input("abc", "hello")
CALL OK(s .EQ. "hello", "longer option")
CALL OK(LEN(s) .EQ. 5_I4B, "length of longer option")

! trailing blanks of the option are kept
s = Input("abc", "xy  ")
CALL OK(LEN(s) .EQ. 4_I4B, "trailing blanks are kept")

! an empty option is still an option
s = Input("abc", "")
CALL OK(LEN(s) .EQ. 0_I4B, "empty option")

END PROGRAM main
