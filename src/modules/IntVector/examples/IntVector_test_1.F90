!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Shape of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Shape
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj
INTEGER(I4B) :: s(1)

s = Shape(obj)
CALL OK(s(1) .EQ. 0, "Shape: empty IntVector_")

obj = [1_I4B, 2_I4B, 3_I4B]
s = Shape(obj)
CALL OK(s(1) .EQ. 3, "Shape: [3] after obj = [1, 2, 3]")

END PROGRAM main
