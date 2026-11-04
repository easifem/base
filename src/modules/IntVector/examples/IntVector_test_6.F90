!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Reallocate for an array of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Reallocate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_), ALLOCATABLE :: obj(:)

CALL Reallocate(obj=obj, row=3_I4B)
CALL OK(SIZE(obj) .EQ. 3, "Reallocate: 3 IntVector_ entries")
CALL OK(Size(obj(1)) .EQ. 0, "Reallocate: each entry is empty")

CALL Reallocate(obj=obj, row=5_I4B)
CALL OK(SIZE(obj) .EQ. 5, "Reallocate: resized to 5 entries")

END PROGRAM main
