!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector(val) constructor for real val

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE IntVector_Method, ONLY: IntVector
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = IntVector([2.9_DFP, -0.5_DFP])
CALL OK(Size(obj) .EQ. 2, "IntVector(real val): Size is 2")
CALL OK(ALL(obj%val(1:2) .EQ. [2, 0]), &
        "IntVector(real val): truncated toward zero")

END PROGRAM main
