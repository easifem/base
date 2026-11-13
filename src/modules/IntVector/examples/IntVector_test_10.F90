!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing obj = val for real val (truncation toward zero)

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = [1.7_DFP, -1.7_DFP, 3.0_DFP]
CALL OK(Size(obj) .EQ. 3, "obj = real val: Size is 3")
CALL OK(ALL(obj%val(1:3) .EQ. [1, -1, 3]), &
        "obj = real val: truncated toward zero")

END PROGRAM main
