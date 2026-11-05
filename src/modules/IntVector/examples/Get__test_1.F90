!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get_ for the whole of one IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: TypeIntVector
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Get_
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, empty, ans
INTEGER(I4B), ALLOCATABLE :: arr(:)

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! Test 1: the result is an IntVector_ copy
CALL Get_(obj=obj, ans=ans)
CALL OK(SIZE(ans) .EQ. 4, "Get_: ans was not allocated, size(ans).")
CALL OK(ALL(ans%val(1:4) .EQ. [10, 20, 30, 40]), &
        "Get: ans was not allocated, value :")

! Test 2: the result is an integer array
arr = Get(obj, 1_I4B)
CALL OK(SIZE(arr) .EQ. 4, "Get: array result, SIZE")
CALL OK(ALL(arr .EQ. [10, 20, 30, 40]), "Get: array result, values")

! Test 3: an empty obj gives an empty result
ans = Get(empty, TypeIntVector)
CALL OK(SIZE(ans) .EQ. 0, "Get: empty obj, IntVector_ result")
arr = Get(empty, 1_I4B)
CALL OK(SIZE(arr) .EQ. 0, "Get: empty obj, array result")

! Test 4: entries beyond tsize are not returned
obj%tsize = 2
ans = Get(obj, TypeIntVector)
CALL OK(SIZE(ans) .EQ. 2, "Get: spare capacity, Size")
CALL OK(Capacity(ans) .EQ. 2, "Get: result capacity = tsize")
arr = Get(obj, 1_I4B)
CALL OK(ALL(arr .EQ. [10, 20]), "Get: spare capacity, array result")

END PROGRAM main
