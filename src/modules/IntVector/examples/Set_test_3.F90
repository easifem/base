!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Set of all active entries to a constant

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Set
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! Test 1: every active entry is set
BLOCK
  CALL Set(obj, 5_I4B)
  CALL OK(ALL(obj%val .EQ. 5), "Set: constant value")
  CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
END BLOCK

! Test 2: entries beyond tsize are not changed
BLOCK
  obj%tsize = 2
  CALL Set(obj, 8_I4B)
  CALL OK(ALL(obj%val .EQ. [8, 8, 5, 5]), "Set: spare capacity")
END BLOCK

! Test 3: an unallocated obj stays unallocated
BLOCK
  TYPE(IntVector_) :: empty
  CALL Set(empty, 5_I4B)
  CALL OK(.NOT. ALLOCATED(empty%val), "Set: unallocated obj")
END BLOCK

END PROGRAM main
