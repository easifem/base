!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IsAllocated and IsInitiated

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: DEALLOCATE
USE IntVector_Method, ONLY: IsAllocated
USE IntVector_Method, ONLY: IsInitiated
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

! Test 1: a new IntVector_ is not allocated
CALL OK(.NOT. IsAllocated(obj), "IsAllocated: new obj")
CALL OK(.NOT. IsInitiated(obj), "IsInitiated: new obj")

! Test 2: allocated after assignment
obj = [1_I4B, 2_I4B, 3_I4B]
CALL OK(IsAllocated(obj), "IsAllocated: after obj = [1, 2, 3]")
CALL OK(IsInitiated(obj), "IsInitiated: after obj = [1, 2, 3]")

! Test 3: not allocated after Deallocate
CALL DEALLOCATE (obj)
CALL OK(.NOT. IsAllocated(obj), "IsAllocated: after Deallocate")
CALL OK(.NOT. IsInitiated(obj), "IsInitiated: after Deallocate")

END PROGRAM main
