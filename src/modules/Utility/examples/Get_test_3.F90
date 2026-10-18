!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get a strided section of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: Get
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: val(6)

val = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B, 60_I4B]

! Get(val, istart, iend, stride) is val(istart:iend:stride)
CALL OK(ALL(Get(val, 1_I4B, 5_I4B, 2_I4B) .EQ. &
            [10_I4B, 30_I4B, 50_I4B]), "val(1:5:2)")
CALL OK(ALL(Get(val, 2_I4B, 6_I4B, 2_I4B) .EQ. &
            [20_I4B, 40_I4B, 60_I4B]), "val(2:6:2)")
CALL OK(ALL(Get(val, 2_I4B, 4_I4B, 1_I4B) .EQ. val(2:4)), "val(2:4)")

! iend need not be reached exactly
CALL OK(SIZE(Get(val, 1_I4B, 6_I4B, 2_I4B)) .EQ. 3_I4B, "val(1:6:2)")

! negative stride
CALL OK(ALL(Get(val, 6_I4B, 1_I4B, -2_I4B) .EQ. &
            [60_I4B, 40_I4B, 20_I4B]), "val(6:1:-2)")

END PROGRAM main
