!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing DEALLOCATE for a box and a vector of boxes

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: lim(6) = [0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  2.0_DFP, 0.0_DFP, 3.0_DFP]

! Test 1: DEALLOCATE resets a scalar box to zero
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=3_I4B, lim=lim)
  CALL DEALLOCATE (obj)
  CALL OK(obj%nsd .EQ. 0, "DEALLOCATE: nsd is zero")
  CALL OK(ALL(obj%box .EQ. 0.0_DFP), "DEALLOCATE: box is zero")
  CALL OK(ALL(obj%l .EQ. 0.0_DFP), "DEALLOCATE: lengths are zero")
END BLOCK

! Test 2: DEALLOCATE frees a vector of boxes
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  ALLOCATE (obj(2))
  obj(1) = BoundingBox(nsd=3_I4B, lim=lim)
  obj(2) = BoundingBox(nsd=2_I4B, lim=lim)
  CALL DEALLOCATE (obj)
  CALL OK(.NOT. ALLOCATED(obj), "DEALLOCATE: vector is deallocated")
END BLOCK

! Test 3: DEALLOCATE on an unallocated vector does nothing
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL DEALLOCATE (obj)
  CALL OK(.NOT. ALLOCATED(obj), "DEALLOCATE: unallocated vector")
END BLOCK

END PROGRAM main
