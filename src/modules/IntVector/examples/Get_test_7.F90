!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get with a triplet that selects no entries
!
! Such a triplet should give an empty result, as obj%val(3:2:2) does.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: TypeIntVector
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, vec(2), ans
INTEGER(I4B), ALLOCATABLE :: arr(:)

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
vec(1) = [1_I4B, 2_I4B, 3_I4B]
vec(2) = [4_I4B, 5_I4B, 6_I4B]

! Test 1: one IntVector_, IntVector_ result
ans = Get(obj, 3_I4B, 2_I4B, 2_I4B, TypeIntVector)
CALL OK(Size(ans) .EQ. 0, "Get: 3:2:2, IntVector_ result")
ans = Get(obj, 3_I4B, 1_I4B, 1_I4B, TypeIntVector)
CALL OK(Size(ans) .EQ. 0, "Get: 3:1:1, IntVector_ result")

! Test 2: one IntVector_, array result
arr = Get(obj, 3_I4B, 2_I4B, 2_I4B, 1_I4B)
CALL OK(SIZE(arr) .EQ. 0, "Get: 3:2:2, array result")
arr = Get(obj, 3_I4B, 1_I4B, 1_I4B, 1_I4B)
CALL OK(SIZE(arr) .EQ. 0, "Get: 3:1:1, array result")

! Test 3: vector of IntVector_
ans = Get(vec, 3_I4B, 2_I4B, 2_I4B, TypeIntVector)
CALL OK(Size(ans) .EQ. 0, "Get: vector obj, 3:2:2, IntVector_ result")
arr = Get(vec, 3_I4B, 2_I4B, 2_I4B, 1_I4B)
CALL OK(SIZE(arr) .EQ. 0, "Get: vector obj, 3:2:2, array result")

END PROGRAM main
