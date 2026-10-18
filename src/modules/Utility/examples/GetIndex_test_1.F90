!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetIndex for an integer scalar
!
! Description:
!   Includes the regression test for bug H-07 in
!   bug-reports/Utility_bug_report.md: a value that is not in obj
!   gave the index of the nearest entry instead of zero.

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

! a value that is not in obj gives zero
CALL OK(GetIndex(obj, 31_I4B) .EQ. 0_I4B, "31 is not found")
CALL OK(GetIndex(obj, 0_I4B) .EQ. 0_I4B, "0 is not found")
CALL OK(GetIndex(obj, 99_I4B) .EQ. 0_I4B, "99 is not found")

! the same result as the vector version
CALL OK(ALL(GetIndex(obj, [30_I4B, 31_I4B]) .EQ. [3_I4B, 0_I4B]), &
        "same as GetIndex(obj, val(:))")

END PROGRAM main
