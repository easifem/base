!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Size of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL OK(Size(obj) .EQ. 0, "Size: empty IntVector_")

obj = [1_I4B, 2_I4B, 3_I4B]
CALL OK(Size(obj) .EQ. 3, "Size: 3 after obj = [1, 2, 3]")

END PROGRAM main
