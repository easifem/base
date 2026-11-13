!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get that joins a vector of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: TypeIntVector
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj(3), ans
INTEGER(I4B), ALLOCATABLE :: arr(:)

obj(1) = [1_I4B, 2_I4B]
obj(3) = [3_I4B, 4_I4B, 5_I4B]

! Test 1: entries are joined in order, an empty item is skipped
ans = Get(obj, TypeIntVector)
CALL OK(Size(ans) .EQ. 5, "Get: join, IntVector_ result, Size")
CALL OK(ALL(ans%val(1:5) .EQ. [1, 2, 3, 4, 5]), &
        "Get: join, IntVector_ result, values")

! Test 2: array result
arr = Get(obj, 1_I4B)
CALL OK(SIZE(arr) .EQ. 5, "Get: join, array result, SIZE")
CALL OK(ALL(arr .EQ. [1, 2, 3, 4, 5]), "Get: join, array result, values")

! Test 3: entries beyond tsize are not joined
obj(3)%tsize = 1
arr = Get(obj, 1_I4B)
CALL OK(ALL(arr .EQ. [1, 2, 3]), "Get: join, spare capacity, array")
ans = Get(obj, TypeIntVector)
CALL OK(Size(ans) .EQ. 3, "Get: join, spare capacity, IntVector_")

END PROGRAM main
