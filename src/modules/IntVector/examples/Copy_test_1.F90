!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Copy(obj, obj2) of IntVector_ with spare capacity
!
! Copy and `obj = obj2` copy only the active entries of `obj2`. The
! result has `Capacity(obj) = Size(obj2)`, whatever `obj` held before.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Copy
USE IntVector_Method, ONLY: GetTotalDimension
USE IntVector_Method, ONLY: SetTotalDimension
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: spare capacity of obj2 is not copied
BLOCK
  TYPE(IntVector_) :: obj, obj2
  CALL Append(obj2, 4_I4B)
  CALL Append(obj2, 5_I4B)
  CALL OK(Capacity(obj2) .GT. Size(obj2), "Copy: obj2 has spare room")
  CALL Copy(obj, obj2)
  CALL OK(Size(obj) .EQ. 2, "Copy: spare, Size")
  CALL OK(Capacity(obj) .EQ. 2, "Copy: spare, Capacity")
  CALL OK(ALL(obj%val .EQ. [4, 5]), "Copy: spare, values")
END BLOCK

! Test 2: a larger obj is shrunk to the size of obj2
BLOCK
  TYPE(IntVector_) :: obj, obj2
  obj = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]
  obj2 = [9_I4B]
  obj = obj2
  CALL OK(Size(obj) .EQ. 1, "obj = obj2: larger obj, Size")
  CALL OK(SIZE(obj%val) .EQ. 1, "obj = obj2: larger obj, SIZE(val)")
  CALL OK(obj%val(1) .EQ. 9, "obj = obj2: larger obj, value")
END BLOCK

! Test 3: the total dimension is copied
BLOCK
  TYPE(IntVector_) :: obj, obj2
  obj2 = [1_I4B, 2_I4B]
  CALL SetTotalDimension(obj2, 3_I4B)
  CALL Copy(obj, obj2)
  CALL OK(GetTotalDimension(obj) .EQ. 3, "Copy: tDimension")
END BLOCK

! Test 4: an allocated obj2 with no entries gives an empty obj
BLOCK
  TYPE(IntVector_) :: obj, obj2
  INTEGER(I4B) :: none(0)
  obj = [1_I4B, 2_I4B]
  obj2 = none
  CALL Copy(obj, obj2)
  CALL OK(Size(obj) .EQ. 0, "Copy: zero-size obj2, Size")
  CALL OK(Capacity(obj) .EQ. 0, "Copy: zero-size obj2, Capacity")
END BLOCK

END PROGRAM main
