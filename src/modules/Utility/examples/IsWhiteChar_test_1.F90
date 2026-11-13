!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IsWhiteChar is true for a blank or a tab

PROGRAM main
USE StringUtility, ONLY: IsWhiteChar
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(IsWhiteChar(" "), "blank")
CALL OK(IsWhiteChar(ACHAR(9)), "tab")
CALL OK(.NOT. IsWhiteChar("a"), "letter")
CALL OK(.NOT. IsWhiteChar("_"), "underscore")
CALL OK(.NOT. IsWhiteChar(ACHAR(10)), "new line is not white")
CALL OK(.NOT. IsWhiteChar(ACHAR(13)), "carriage return is not white")

END PROGRAM main
