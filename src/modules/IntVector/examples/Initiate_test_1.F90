!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Initiate of IntVector_ on objects that hold data
!
! Initiate discards the old data. Each case checks that Size, Capacity
! and the values match the new request.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Initiate
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Initiate(obj, tSize) resets an object with spare capacity
BLOCK
  TYPE(IntVector_) :: obj
  CALL Append(obj, 7_I4B)
  CALL Initiate(obj=obj, tSize=3_I4B)
  CALL OK(Size(obj) .EQ. 3, "Initiate(tSize): Size")
  CALL OK(Capacity(obj) .EQ. 3, "Initiate(tSize): Capacity")
  CALL OK(ALL(obj%val .EQ. 0), "Initiate(tSize): old data is zeroed")
END BLOCK

! Test 2: Initiate(obj, tSize=0) gives an allocated empty object
BLOCK
  TYPE(IntVector_) :: obj
  obj = [1_I4B, 2_I4B]
  CALL Initiate(obj=obj, tSize=0_I4B)
  CALL OK(Size(obj) .EQ. 0, "Initiate(0): Size")
  CALL OK(ALLOCATED(obj%val), "Initiate(0): val is allocated")
END BLOCK

! Test 3: obj = val with a zero-size array
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: none(0)
  obj = [1_I4B, 2_I4B]
  obj = none
  CALL OK(Size(obj) .EQ. 0, "obj = []: Size")
  CALL OK(Capacity(obj) .EQ. 0, "obj = []: Capacity")
END BLOCK

! Test 4: array Initiate on an array of the same size resets each item
BLOCK
  TYPE(IntVector_), ALLOCATABLE :: obj(:)
  CALL Initiate(obj=obj, tSize=[2_I4B, 2_I4B])
  obj(1)%val = [5_I4B, 6_I4B]
  CALL Initiate(obj=obj, tSize=[1_I4B, 3_I4B])
  CALL OK(SIZE(obj) .EQ. 2, "Initiate(tSize(:)): same SIZE(obj)")
  CALL OK(Size(obj(1)) .EQ. 1, "Initiate(tSize(:)): Size(obj(1))")
  CALL OK(Size(obj(2)) .EQ. 3, "Initiate(tSize(:)): Size(obj(2))")
  CALL OK(obj(1)%val(1) .EQ. 0, "Initiate(tSize(:)): old data zeroed")
END BLOCK

! Test 5: array Initiate on an array of another size reallocates it
BLOCK
  TYPE(IntVector_), ALLOCATABLE :: obj(:)
  CALL Initiate(obj=obj, tSize=[2_I4B])
  CALL Initiate(obj=obj, tSize=[1_I4B, 1_I4B, 1_I4B])
  CALL OK(SIZE(obj) .EQ. 3, "Initiate(tSize(:)): resized")
END BLOCK

END PROGRAM main
