!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IntVector_Pointer(tSize)

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: IntVector_Pointer
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_), POINTER :: obj

obj => IntVector_Pointer(tSize=4_I4B)
CALL OK(ASSOCIATED(obj), "IntVector_Pointer(tSize): associated")
CALL OK(Size(obj) .EQ. 4, "IntVector_Pointer(tSize): Size is 4")
DEALLOCATE (obj)

END PROGRAM main
