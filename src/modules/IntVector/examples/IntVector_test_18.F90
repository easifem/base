!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing val = obj (IntVector_ to integer array)

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, empty
INTEGER(I4B), ALLOCATABLE :: val(:)

obj = [1_I4B, 2_I4B, 3_I4B]
val = obj
CALL OK(SIZE(val) .EQ. 3, "val = obj: SIZE(val) is 3")
CALL OK(ALL(val .EQ. [1, 2, 3]), "val = obj: values")

val = empty
CALL OK(SIZE(val) .EQ. 0, "val = empty obj: SIZE(val) is 0")

END PROGRAM main
