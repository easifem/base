!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IsBlank is true if a string has only blanks and tabs

PROGRAM main
USE StringUtility, ONLY: IsBlank
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=10) :: astr

CALL OK(IsBlank("   "), "blanks")
CALL OK(IsBlank(""), "empty string")
CALL OK(IsBlank(" "//ACHAR(9)//" "), "blanks and a tab")
CALL OK(.NOT. IsBlank(" a "), "a letter")
CALL OK(.NOT. IsBlank(ACHAR(10)), "new line")

astr = " "
CALL OK(IsBlank(astr), "blank variable")

astr = "x"
CALL OK(.NOT. IsBlank(astr), "padded variable with a letter")

END PROGRAM main
