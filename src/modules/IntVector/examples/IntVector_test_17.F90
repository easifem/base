!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector_Pointer(val) for real val

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE IntVector_Method, ONLY: IntVector_Pointer
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_), POINTER :: obj

obj => IntVector_Pointer([1.5_DFP, 2.5_DFP])
CALL OK(Size(obj) .EQ. 2, "IntVector_Pointer(real val): Size is 2")
CALL OK(ALL(obj%val(1:2) .EQ. [1, 2]), &
        "IntVector_Pointer(real val): truncated toward zero")
DEALLOCATE (obj)

END PROGRAM main
