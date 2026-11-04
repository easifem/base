!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing RemoveDuplicates

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

! Test 1: duplicates are removed and the result is sorted
BLOCK
  obj = [5_I4B, 4_I4B, 4_I4B, 3_I4B, 1_I4B, 1_I4B, 1_I4B, 2_I4B]
  CALL RemoveDuplicates(obj)
  CALL OK(SIZE(obj) .EQ. 5, "RemoveDuplicates: Size")
  CALL OK(Capacity(obj) .EQ. 8, "RemoveDuplicates: capacity")
  CALL OK(ALL(obj%val(1:obj%tsize) .EQ. [1, 2, 3, 4, 5]), &
          "RemoveDuplicates: values")
END BLOCK

! Test 2: no duplicates, the vector is only sorted
BLOCK
  obj = [3_I4B, 1_I4B, 2_I4B]
  CALL RemoveDuplicates(obj)
  CALL OK(SIZE(obj) .EQ. 3, "RemoveDuplicates: no duplicates, Size")
  CALL OK(ALL(obj%val(1:obj%tsize) .EQ. [1, 2, 3]), &
          "RemoveDuplicates: no duplicates, values")
END BLOCK

! Test 3: all entries are the same
BLOCK
  obj = [7_I4B, 7_I4B, 7_I4B]
  CALL RemoveDuplicates(obj)
  CALL OK(SIZE(obj) .EQ. 1, "RemoveDuplicates: all same, Size")
  CALL OK(obj%val(1) .EQ. 7, "RemoveDuplicates: all same, value")
END BLOCK

! Test 4: one entry
BLOCK
  obj = [7_I4B]
  CALL RemoveDuplicates(obj)
  CALL OK(SIZE(obj) .EQ. 1, "RemoveDuplicates: one entry, Size")
  CALL OK(obj%val(1) .EQ. 7, "RemoveDuplicates: one entry, value")
END BLOCK

! Test 5: an unallocated obj stays unallocated
BLOCK
  TYPE(IntVector_) :: empty
  CALL RemoveDuplicates(empty)
  CALL OK(.NOT. ALLOCATED(empty%val), "RemoveDuplicates: unallocated")
  CALL OK(SIZE(empty) .EQ. 0, "RemoveDuplicates: unallocated, Size")
END BLOCK

END PROGRAM main
