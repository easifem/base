!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Set with a list of positions

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

! Test 1: each position gets its own value
BLOCK
  CALL Set(obj, [1_I4B, 3_I4B], [-1_I4B, -3_I4B])
  CALL OK(ALL(obj%val .EQ. [-1, 20, -3, 40]), "Set: list of values")
END BLOCK

! Test 2: positions can be in any order
BLOCK
  CALL Set(obj, [4_I4B, 2_I4B], [44_I4B, 22_I4B])
  CALL OK(ALL(obj%val .EQ. [-1, 22, -3, 44]), "Set: unsorted positions")
END BLOCK

! Test 3: a single value is written to every position
BLOCK
  CALL Set(obj, [1_I4B, 2_I4B, 4_I4B], [7_I4B])
  CALL OK(ALL(obj%val .EQ. [7, 7, -3, 7]), "Set: single value")
END BLOCK

! Test 4: an empty list of positions changes nothing
BLOCK
  INTEGER(I4B) :: none(0)
  CALL Set(obj, none, none)
  CALL OK(ALL(obj%val .EQ. [7, 7, -3, 7]), "Set: no positions")
END BLOCK

! Test 5: the size does not change
BLOCK
  CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
END BLOCK

END PROGRAM main
