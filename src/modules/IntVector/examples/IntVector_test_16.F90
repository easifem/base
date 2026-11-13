!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector_Pointer(val) for integer val

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: IntVector_Pointer
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_), POINTER :: obj

obj => IntVector_Pointer([1_I4B, 2_I4B])
CALL OK(Size(obj) .EQ. 2, "IntVector_Pointer(val): Size is 2")
CALL OK(ALL(obj%val(1:2) .EQ. [1, 2]), "IntVector_Pointer(val): values")
DEALLOCATE (obj)

END PROGRAM main
