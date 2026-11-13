!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector(val) constructor for integer val

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: IntVector
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = IntVector([3_I4B, 1_I4B, 2_I4B])
CALL OK(Size(obj) .EQ. 3, "IntVector(val): Size is 3")
CALL OK(ALL(obj%val(1:3) .EQ. [3, 1, 2]), "IntVector(val): values")

END PROGRAM main
