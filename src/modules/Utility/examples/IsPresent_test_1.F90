!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IsPresent is true if a pattern is found in a string

PROGRAM main
USE StringUtility, ONLY: IsPresent
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=20) :: astr

CALL OK(IsPresent("Hello world", "world"), "pattern found")
CALL OK(.NOT. IsPresent("Hello world", "World"), "case matters")
CALL OK(.NOT. IsPresent("abc", "abcd"), "pattern too long")
CALL OK(.NOT. IsPresent("abc", ""), "empty pattern")
CALL OK(.NOT. IsPresent("", "a"), "empty string")

! trailing blanks are part of the string
astr = "abc"
CALL OK(IsPresent(astr, "c "), "blank after c in padded string")
CALL OK(.NOT. IsPresent(TRIM(astr), "c "), "trimmed string")

END PROGRAM main
