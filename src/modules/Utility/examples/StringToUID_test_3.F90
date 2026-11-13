!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: StringToUID ignores trailing blanks, empty string, leading blanks

PROGRAM main
USE GlobalData, ONLY: I4B
USE HashingUtility, ONLY: StringToUID
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=32) :: aline

! The string is trimmed before hashing, only trailing blanks are ignored.
CALL OK(StringToUID("") .EQ. 0_I4B, "empty string is 0")
CALL OK(StringToUID("    ") .EQ. 0_I4B, "blank string is 0")
CALL OK(StringToUID("AB  ") .EQ. StringToUID("AB"), "trailing blanks")

aline = "AB"
CALL OK(StringToUID(aline) .EQ. 107_I4B, "padded variable, AB = 107")
CALL OK(StringToUID(TRIM(aline)) .EQ. StringToUID(aline), "TRIM(aline)")

! Leading blanks are part of the string.
! ' ' has weight 32 - 64 = -32, so ' A' = -32 + 1 * 53 = 21
CALL OK(StringToUID(" A") .EQ. 21_I4B, "leading blank, ' A' = 21")
CALL OK(StringToUID(" A") .NE. StringToUID("A"), "leading blank matters")

! The function is deterministic
CALL OK(StringToUID("Hello") .EQ. StringToUID("Hello"), "deterministic")

END PROGRAM main
