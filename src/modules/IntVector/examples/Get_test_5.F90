!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get for selected positions of a vector of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: TypeIntVector
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj(2), ans
INTEGER(I4B), ALLOCATABLE :: arr(:)

obj(1) = [1_I4B, 2_I4B, 3_I4B]
obj(2) = [4_I4B, 5_I4B, 6_I4B]

! Test 1: the same positions are read from each item
ans = Get(obj, [3_I4B, 1_I4B], TypeIntVector)
CALL OK(Size(ans) .EQ. 4, "Get: vector obj, indx, IntVector_, Size")
CALL OK(ALL(ans%val(1:4) .EQ. [3, 1, 6, 4]), &
        "Get: vector obj, indx, IntVector_, values")

! Test 2: array result
arr = Get(obj, [3_I4B, 1_I4B], 1_I4B)
CALL OK(SIZE(arr) .EQ. 4, "Get: vector obj, indx, array, SIZE")
CALL OK(ALL(arr .EQ. [3, 1, 6, 4]), "Get: vector obj, indx, array, values")

END PROGRAM main
