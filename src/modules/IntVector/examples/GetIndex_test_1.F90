!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing GetIndex for a scalar and a vector of values

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: GetIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, empty

obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]

! Test 1: scalar value, first position is returned
CALL OK(GetIndex(obj, 7_I4B) .EQ. 2, "GetIndex: scalar value")
CALL OK(GetIndex(obj, 5_I4B) .EQ. 1, "GetIndex: repeated value")

! Test 2: vector of values, 0 means not found
CALL OK(ALL(GetIndex(obj, [9_I4B, 1_I4B, 5_I4B, 5_I4B]) .EQ. &
            [4, 0, 1, 1]), "GetIndex: vector of values")

! Test 3: empty vector of values gives an empty result
BLOCK
  INTEGER(I4B) :: novalue(0)
  CALL OK(SIZE(GetIndex(obj, novalue)) .EQ. 0, "GetIndex: empty val")
END BLOCK

! Test 4: nothing is found in an empty obj
CALL OK(GetIndex(empty, 1_I4B) .EQ. 0, "GetIndex: empty obj, scalar")
CALL OK(ALL(GetIndex(empty, [1_I4B, 2_I4B]) .EQ. 0), &
        "GetIndex: empty obj, vector")

! Test 5: entries beyond tsize are not searched
obj%tsize = 3
CALL OK(ALL(GetIndex(obj, [5_I4B, 9_I4B]) .EQ. [1, 0]), &
        "GetIndex: vector of values with tsize = 3")

END PROGRAM main
