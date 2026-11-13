!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: StringToUID of strings with characters below 'A', negative values

PROGRAM main
USE GlobalData, ONLY: I4B
USE HashingUtility, ONLY: StringToUID
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Characters whose code is smaller than ICHAR('A') (digits, '*', '-', ...)
! have a negative weight. MOD keeps the sign of the dividend, so the
! result can be negative. The expected values are those of the
! polynomial hash with p = 53 and m = 1000009.
CALL OK(StringToUID("x-2") .EQ. -40277_I4B, "x-2 = -40277")
CALL OK(StringToUID("x-12") .EQ. -127346_I4B, "x-12 = -127346")
CALL OK(StringToUID("x**2") .EQ. -147168_I4B, "x**2 = -147168")
CALL OK(StringToUID("x**12") .EQ. -761789_I4B, "x**12 = -761789")

CALL OK(StringToUID("x**2") .LT. 0_I4B, "x**2 is negative")
CALL OK(StringToUID("x**2") .NE. StringToUID("x**12"), "x**2 /= x**12")
CALL OK(StringToUID("x-2") .NE. StringToUID("x**2"), "x-2 /= x**2")

END PROGRAM main
