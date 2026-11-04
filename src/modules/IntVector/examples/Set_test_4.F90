!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Set of the leading active entries from an array

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

! Test 1: VALUE has the same size as obj
BLOCK
  CALL Set(obj, [1_I4B, 2_I4B, 3_I4B, 4_I4B])
  CALL OK(ALL(obj%val .EQ. [1, 2, 3, 4]), "Set: same size")
END BLOCK

! Test 2: a shorter VALUE sets only the leading entries
BLOCK
  CALL Set(obj, [7_I4B, 8_I4B])
  CALL OK(ALL(obj%val .EQ. [7, 8, 3, 4]), "Set: shorter VALUE")
END BLOCK

! Test 3: a longer VALUE is cut at the size of obj
BLOCK
  CALL Set(obj, [5_I4B, 6_I4B, 7_I4B, 8_I4B, 9_I4B, 10_I4B])
  CALL OK(ALL(obj%val .EQ. [5, 6, 7, 8]), "Set: longer VALUE")
  CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
END BLOCK

! Test 4: an empty VALUE changes nothing
BLOCK
  INTEGER(I4B) :: none(0)
  CALL Set(obj, none)
  CALL OK(ALL(obj%val .EQ. [5, 6, 7, 8]), "Set: empty VALUE")
END BLOCK

! Test 5: entries beyond tsize are not changed
BLOCK
  obj%tsize = 2
  CALL Set(obj, [1_I4B, 2_I4B, 3_I4B])
  CALL OK(ALL(obj%val .EQ. [1, 2, 7, 8]), "Set: spare capacity")
END BLOCK

! Test 6: an unallocated obj stays unallocated
BLOCK
  TYPE(IntVector_) :: empty
  CALL Set(empty, [1_I4B, 2_I4B])
  CALL OK(.NOT. ALLOCATED(empty%val), "Set: unallocated obj")
END BLOCK

END PROGRAM main
