!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing ALLOCATE and DEALLOCATE of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ALLOCATE
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: DEALLOCATE
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL ALLOCATE(obj=obj, dims=4_I4B)
CALL OK(Size(obj) .EQ. 4, "ALLOCATE: Size is 4")
CALL OK(Capacity(obj) .EQ. 4, "ALLOCATE: Capacity is 4")
CALL OK(ALL(obj%val .EQ. 0), "ALLOCATE: entries are zero")

CALL DEALLOCATE(obj)
CALL OK(Size(obj) .EQ. 0, "DEALLOCATE: Size is 0")
CALL OK(Capacity(obj) .EQ. 0, "DEALLOCATE: Capacity is 0")
CALL OK(.NOT. ALLOCATED(obj%val), "DEALLOCATE: val is deallocated")

END PROGRAM main
