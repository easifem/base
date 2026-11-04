!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector(tSize) constructor

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: IntVector
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = IntVector(tSize=4_I4B)
CALL OK(Size(obj) .EQ. 4, "IntVector(tSize): Size is 4")
CALL OK(ALL(obj%val(1:4) .EQ. 0), "IntVector(tSize): entries are zero")

END PROGRAM main
