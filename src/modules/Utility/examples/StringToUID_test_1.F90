!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: StringToUID of letters, the value depends on the letter and order

PROGRAM main
USE GlobalData, ONLY: I4B
USE HashingUtility, ONLY: StringToUID
USE Test_Method, ONLY: OK
IMPLICIT NONE

! The code of a character c is ICHAR(c) - ICHAR('A') + 1, so that
! A = 1, B = 2, ..., Z = 26, a = 33, ..., z = 58.
CALL OK(StringToUID("A") .EQ. 1_I4B, "A = 1")
CALL OK(StringToUID("B") .EQ. 2_I4B, "B = 2")
CALL OK(StringToUID("Z") .EQ. 26_I4B, "Z = 26")
CALL OK(StringToUID("a") .EQ. 33_I4B, "a = 33")
CALL OK(StringToUID("z") .EQ. 58_I4B, "z = 58")

! The base of the polynomial is p = 53
! AB = 1 + 2 * 53, BA = 2 + 1 * 53, AAA = 1 + 53 + 53**2
CALL OK(StringToUID("AB") .EQ. 107_I4B, "AB = 107")
CALL OK(StringToUID("BA") .EQ. 55_I4B, "BA = 55")
CALL OK(StringToUID("AAA") .EQ. 2863_I4B, "AAA = 2863")

CALL OK(StringToUID("AB") .NE. StringToUID("BA"), "order matters")
CALL OK(StringToUID("A") .NE. StringToUID("a"), "case matters")

END PROGRAM main
