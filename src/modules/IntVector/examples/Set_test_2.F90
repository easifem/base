!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Set with one position and one value

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Set
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = [10_I4B, 20_I4B, 30_I4B]

! Test 1: an interior entry
BLOCK
  CALL Set(obj, 2_I4B, -5_I4B)
  CALL OK(ALL(obj%val .EQ. [10, -5, 30]), "Set: interior entry")
END BLOCK

! Test 2: the first and the last entry
BLOCK
  CALL Set(obj, 1_I4B, 0_I4B)
  CALL Set(obj, 3_I4B, 9_I4B)
  CALL OK(ALL(obj%val .EQ. [0, -5, 9]), "Set: first and last entry")
END BLOCK

! Test 3: the size does not change
BLOCK
  CALL OK(Size(obj) .EQ. 3, "Set: Size is not changed")
END BLOCK

END PROGRAM main
