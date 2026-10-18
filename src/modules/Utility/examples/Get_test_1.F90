!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get one entry of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Get
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: val(3)

val = [10_I4B, 20_I4B, 30_I4B]

CALL OK(Get(val, 1_I4B) .EQ. 10_I4B, "first entry")
CALL OK(Get(val=val, indx=3_I4B) .EQ. 30_I4B, "last entry")

! indx larger than SIZE(val) returns zero
CALL OK(Get(val, 4_I4B) .EQ. 0_I4B, "indx out of range")

END PROGRAM main
