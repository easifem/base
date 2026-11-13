!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: NumStrings counts the blank separated words in a string

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: NumStrings
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=40) :: astr

astr = "Hello world how are you!"
CALL OK(NumStrings(astr) .EQ. 5_I4B, "five words")

astr = "Hello world, how are you"
CALL OK(NumStrings(astr) .EQ. 5_I4B, "a comma is part of a word")

astr = "  a  b   c  "
CALL OK(NumStrings(astr) .EQ. 3_I4B, "many blanks")

astr = "a"//ACHAR(9)//"b"
CALL OK(NumStrings(astr) .EQ. 2_I4B, "tab is a separator")

astr = "'a b' c"
CALL OK(NumStrings(astr) .EQ. 2_I4B, "quoted words count once")

astr = "a 2*b c"
CALL OK(NumStrings(astr) .EQ. 4_I4B, "repeat count 2*b")

astr = " "
CALL OK(NumStrings(astr) .EQ. 0_I4B, "blank string")

! an unmatched quote is an error
astr = "a 'b"
CALL OK(NumStrings(astr) .EQ. -1_I4B, "odd number of ' quotes")

astr = 'a "b'
CALL OK(NumStrings(astr) .EQ. -2_I4B, 'odd number of " quotes')

END PROGRAM main
