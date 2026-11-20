!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing how Append grows the capacity of IntVector_
!
! A full `obj%val` grows by a fixed chunk of 10 entries. This test
! checks the capacity after each growth step.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: the first append allocates 10 entries
BLOCK
  TYPE(IntVector_) :: obj
  CALL Append(obj, 1_I4B)
  CALL OK(Capacity(obj) .EQ. 10, "Append: first chunk is 10")
  CALL OK(Size(obj) .EQ. 1, "Append: first chunk, Size")
END BLOCK

! Test 2: the 11th append grows the capacity from 10 to 20
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: ii
  DO ii = 1, 11
    CALL Append(obj, ii)
  END DO
  CALL OK(Capacity(obj) .EQ. 20, "Append: 11 entries, capacity 20")
END BLOCK

! Test 3: the 21st append grows the capacity from 20 to 30
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: ii
  DO ii = 1, 21
    CALL Append(obj, ii)
  END DO
  CALL OK(Size(obj) .EQ. 21, "Append: 21 entries, Size")
  CALL OK(Capacity(obj) .EQ. 30, "Append: 21 entries, capacity 30")
END BLOCK

! Test 4: an array append on a full obj grows by SIZE(VALUE)
BLOCK
  TYPE(IntVector_) :: obj
  obj = [1_I4B, 2_I4B]
  CALL Append(obj, [3_I4B, 4_I4B, 5_I4B])
  CALL OK(Capacity(obj) .EQ. 5, "Append: array, capacity 5")
END BLOCK

! Test 5: capacity always equals SIZE(obj%val)
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: ii
  LOGICAL :: isok
  isok = .TRUE.
  DO ii = 1, 50
    CALL Append(obj, ii)
    isok = isok .AND. (Capacity(obj) .EQ. SIZE(obj%val))
  END DO
  CALL OK(isok, "Append: capacity = SIZE(val) after each call")
END BLOCK

END PROGRAM main
