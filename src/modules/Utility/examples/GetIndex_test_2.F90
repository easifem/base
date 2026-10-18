!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetIndex for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: obj(4)
INTEGER(I4B), ALLOCATABLE :: ans(:)

obj = [10_I4B, 20_I4B, 30_I4B, 20_I4B]

! ans(j) is the first index of val(j) in obj, zero if not found
ans = GetIndex(obj, [20_I4B, 40_I4B, 10_I4B])
CALL OK(SIZE(ans) .EQ. 3_I4B, "size of the result")
CALL OK(ALL(ans .EQ. [2_I4B, 0_I4B, 1_I4B]), "indices")

! no value is found
ans = GetIndex(obj, [1_I4B, 2_I4B])
CALL OK(ALL(ans .EQ. 0_I4B), "no value is found")

END PROGRAM main
