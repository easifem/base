!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing SetTotalDimension

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: GetTotalDimension
USE IntVector_Method, ONLY: SetTotalDimension
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

! Test 1: the default value is 1
BLOCK
  CALL OK(GetTotalDimension(obj) .EQ. 1, "SetTotalDimension: default")
END BLOCK

! Test 2: an unallocated obj can be set
BLOCK
  CALL SetTotalDimension(obj, 3_I4B)
  CALL OK(GetTotalDimension(obj) .EQ. 3, "SetTotalDimension: unallocated")
END BLOCK

! Test 3: the data is not changed
BLOCK
  obj = [10_I4B, 20_I4B, 30_I4B]
  CALL SetTotalDimension(obj, 2_I4B)
  CALL OK(GetTotalDimension(obj) .EQ. 2, "SetTotalDimension: new value")
  CALL OK(Size(obj) .EQ. 3, "SetTotalDimension: Size is not changed")
  CALL OK(ALL(obj%val .EQ. [10, 20, 30]), &
          "SetTotalDimension: values are not changed")
END BLOCK

END PROGRAM main
