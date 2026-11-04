!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Capacity of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Initiate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL OK(Capacity(obj) .EQ. 0, "Capacity: empty IntVector_")

CALL Initiate(obj=obj, tSize=5_I4B)
CALL OK(Capacity(obj) .EQ. 5, "Capacity: 5 after Initiate")
CALL OK(Capacity(obj) .GE. Size(obj), "Capacity: not less than Size")

END PROGRAM main
