!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Reallocate for a vector of BoundingBox_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: Reallocate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Reallocate an unallocated vector
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL Reallocate(obj, 3_I4B)
  CALL OK(ALLOCATED(obj), "Reallocate: unallocated, allocated")
  CALL OK(SIZE(obj) .EQ. 3, "Reallocate: unallocated, size")
END BLOCK

! Test 2: Reallocate to a new size
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL Reallocate(obj, 3_I4B)
  CALL Reallocate(obj, 5_I4B)
  CALL OK(SIZE(obj) .EQ. 5, "Reallocate: grow, size")
  CALL Reallocate(obj, 2_I4B)
  CALL OK(SIZE(obj) .EQ. 2, "Reallocate: shrink, size")
END BLOCK

! Test 3: Reallocate to the same size keeps the values
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL Reallocate(obj, 2_I4B)
  obj(1) = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                       1.0_DFP, 0.0_DFP, 0.0_DFP])
  CALL Reallocate(obj, 2_I4B)
  CALL OK(SIZE(obj) .EQ. 2, "Reallocate: same size, size")
  CALL OK(obj(1)%nsd .EQ. 2, "Reallocate: same size, values kept")
END BLOCK

END PROGRAM main
