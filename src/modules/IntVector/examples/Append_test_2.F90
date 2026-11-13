!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Append with empty input
!
! An empty input should leave the object unchanged, also when the object
! has never been allocated.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: DEALLOCATE
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: empty(0)

! Test 1: empty array, object with values
BLOCK
  TYPE(IntVector_) :: obj
  obj = [1_I4B, 2_I4B]
  CALL Append(obj, empty)
  CALL OK(Size(obj) .EQ. 2, "Append: empty array, tsize")
  CALL OK(obj%capacity .EQ. 2, "Append: empty array, capacity")
END BLOCK

! Test 2: empty IntVector_, object with values
BLOCK
  TYPE(IntVector_) :: obj, other
  obj = [1_I4B, 2_I4B]
  CALL Append(obj, other)
  CALL OK(Size(obj) .EQ. 2, "Append: empty IntVector_, tsize")
  CALL OK(ALL(obj%val(1:2) .EQ. [1, 2]), "Append: empty IntVector_, val")
END BLOCK

! Test 3: empty IntVector_, new object
BLOCK
  TYPE(IntVector_) :: obj, other
  CALL Append(obj, other)
  CALL OK(Size(obj) .EQ. 0, "Append: both empty, tsize")
  CALL OK(obj%capacity .EQ. 0, "Append: both empty, capacity")
END BLOCK

! Test 4: empty array, new object
BLOCK
  TYPE(IntVector_) :: obj
  CALL Append(obj, empty)
  CALL OK(Size(obj) .EQ. 0, "Append: empty array, new obj, tsize")
  CALL OK(obj%capacity .EQ. 0, "Append: empty array, new obj, capacity")
END BLOCK

! Test 5: empty array, deallocated object
BLOCK
  TYPE(IntVector_) :: obj
  obj = [1_I4B, 2_I4B, 3_I4B]
  CALL DEALLOCATE(obj)
  CALL Append(obj, empty)
  CALL OK(Size(obj) .EQ. 0, "Append: empty array, dealloc obj, tsize")
  CALL OK(obj%capacity .EQ. 0, "Append: empty array, dealloc obj, capacity")
END BLOCK

END PROGRAM main
