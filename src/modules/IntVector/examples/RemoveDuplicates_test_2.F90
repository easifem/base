!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing RemoveDuplicates with spare capacity
!
! Entries beyond tsize are not active, so they should not enter the result.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: RemoveDuplicates
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = [3_I4B, 1_I4B, 2_I4B, 1_I4B, 9_I4B, 8_I4B]
obj%tsize = 4

! Test 1: only the active entries are used
BLOCK
  CALL RemoveDuplicates(obj)
  CALL OK(SIZE(obj) .EQ. 3, "RemoveDuplicates: spare capacity, Size")
  CALL OK(Capacity(obj) .EQ. 6, "RemoveDuplicates: spare capacity, capacity")
  CALL OK(ALL(obj%val(1:obj%tsize) .LE. 3), &
          "RemoveDuplicates: spare capacity, inactive entries dropped")
END BLOCK

END PROGRAM main
