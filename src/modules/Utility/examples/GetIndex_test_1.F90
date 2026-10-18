!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetIndex for an integer scalar

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: obj(4), obj2(3)

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

CALL OK(GetIndex(obj, 30_I4B) .EQ. 3_I4B, "index of 30")
CALL OK(GetIndex(obj, 10_I4B) .EQ. 1_I4B, "index of 10")
CALL OK(GetIndex(obj=obj, val=40_I4B) .EQ. 4_I4B, "index of 40")

! the first index is returned for a repeated value
obj2 = [5_I4B, 7_I4B, 5_I4B]
CALL OK(GetIndex(obj2, 5_I4B) .EQ. 1_I4B, "first index of 5")

END PROGRAM main
