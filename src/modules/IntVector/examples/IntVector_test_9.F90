!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Initiate(obj, val) and obj = val for integer val

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Initiate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL Initiate(obj=obj, val=[1_I4B, 2_I4B, 3_I4B])
CALL OK(Size(obj) .EQ. 3, "Initiate: Size is 3")
CALL OK(ALL(obj%val(1:3) .EQ. [1, 2, 3]), "Initiate: values")

obj = [4_I4B, 5_I4B]
CALL OK(Size(obj) .EQ. 2, "obj = val: Size is 2")
CALL OK(ALL(obj%val(1:2) .EQ. [4, 5]), "obj = val: values")

END PROGRAM main
