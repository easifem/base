!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing obj = obj2 and Copy(obj, obj2) of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Copy
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, obj2, empty

obj2 = [1_I4B, 2_I4B, 3_I4B]

obj = obj2
CALL OK(Size(obj) .EQ. 3, "obj = obj2: Size is 3")
CALL OK(Capacity(obj) .EQ. 3, "obj = obj2: Capacity is 3")
CALL OK(ALL(obj%val(1:3) .EQ. [1, 2, 3]), "obj = obj2: values")

CALL Copy(obj, empty)
CALL OK(Size(obj) .EQ. 0, "Copy from empty: Size is 0")
CALL OK(.NOT. ALLOCATED(obj%val), "Copy from empty: val deallocated")

END PROGRAM main
