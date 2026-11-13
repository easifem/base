!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Deallocate a vector of String

PROGRAM main
USE String_Class, ONLY: String
USE StringUtility, ONLY: DEALLOCATE
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(String), ALLOCATABLE :: obj(:)

ALLOCATE (obj(3))
obj(1) = "a"
obj(2) = "bb"
obj(3) = "ccc"

CALL DEALLOCATE(obj)
CALL OK(.NOT. ALLOCATED(obj), "obj is deallocated")

! calling it again on an unallocated vector is safe
CALL DEALLOCATE(obj)
CALL OK(.NOT. ALLOCATED(obj), "unallocated obj is left alone")

END PROGRAM main
