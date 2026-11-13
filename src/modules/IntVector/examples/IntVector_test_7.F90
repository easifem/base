!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Initiate(obj, tSize) of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Initiate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL Initiate(obj=obj, tSize=5_I4B)
CALL OK(Size(obj) .EQ. 5, "Initiate: Size is 5")
CALL OK(ALL(obj%val(1:5) .EQ. 0), "Initiate: entries are zero")

END PROGRAM main
