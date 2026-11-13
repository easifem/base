!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Initiate(obj(:), tSize(:)) of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Initiate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_), ALLOCATABLE :: obj(:)

CALL Initiate(obj=obj, tSize=[2_I4B, 0_I4B, 3_I4B])
CALL OK(SIZE(obj) .EQ. 3, "Initiate: 3 IntVector_ entries")
CALL OK(Size(obj(1)) .EQ. 2, "Initiate: Size(obj(1)) is 2")
CALL OK(Size(obj(2)) .EQ. 0, "Initiate: Size(obj(2)) is 0")
CALL OK(Size(obj(3)) .EQ. 3, "Initiate: Size(obj(3)) is 3")

END PROGRAM main
