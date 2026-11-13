!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Real2Str converts a real number to a string

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Real2Str
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: rtol = 1.0E-6_DFP
CHARACTER(LEN=20) :: s
REAL(DFP) :: r

! format is G17.7, the result is left adjusted
s = Real2Str(1.5_DFP)
CALL OK(TRIM(s) .EQ. "1.500000", "Real2Str(1.5)")
CALL OK(LEN(Real2Str(1.5_DFP)) .EQ. 20_I4B, "length is 20")
CALL OK(s(1:1) .NE. " ", "left adjusted")

! reading the string back gives the value to 7 significant digits
s = Real2Str(1.0_DFP / 3.0_DFP)
READ (s, *) r
CALL OK(ABS(r - 1.0_DFP / 3.0_DFP) .LT. rtol, "1/3 read back")

s = Real2Str(-2.25_DFP)
READ (s, *) r
CALL OK(ABS(r + 2.25_DFP) .LT. rtol, "-2.25 read back")

! large values use the exponent form
s = Real2Str(1.0E10_DFP)
READ (s, *) r
CALL OK(ABS(r - 1.0E10_DFP) .LT. rtol * 1.0E10_DFP, "1e10 read back")
CALL OK(INDEX(s, "E") .GT. 0, "1e10 has an exponent")

END PROGRAM main
