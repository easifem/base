!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get several entries of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Get
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: val(4)

val = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

CALL OK(ALL(Get(val, [3_I4B, 1_I4B]) .EQ. [30_I4B, 10_I4B]), &
        "entries 3 and 1")

! repeated indices are allowed
CALL OK(ALL(Get(val, [2_I4B, 2_I4B]) .EQ. [20_I4B, 20_I4B]), &
        "repeated indices")

! the size of the result is the size of indx
CALL OK(SIZE(Get(val, [1_I4B, 2_I4B, 4_I4B])) .EQ. 3_I4B, &
        "size of the result")

END PROGRAM main
