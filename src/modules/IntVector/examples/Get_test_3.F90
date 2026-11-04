!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get with an index triplet on one IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: TypeIntVector
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, ans
INTEGER(I4B), ALLOCATABLE :: arr(:)

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B]

! Test 1: full triplet, IntVector_ result
ans = Get(obj, 1_I4B, 5_I4B, 2_I4B, TypeIntVector)
CALL OK(Size(ans) .EQ. 3, "Get: triplet, IntVector_ result, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [10, 30, 50]), &
        "Get: triplet, IntVector_ result, values")

! Test 2: iend and stride default to Size(obj) and 1
ans = Get(obj, istart=3_I4B, datatype=TypeIntVector)
CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [30, 40, 50]), &
        "Get: default iend and stride")

! Test 3: stride alone
ans = Get(obj, istart=2_I4B, stride=2_I4B, datatype=TypeIntVector)
CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [20, 40]), "Get: default iend")

! Test 4: negative stride walks backwards
ans = Get(obj, 5_I4B, 1_I4B, -2_I4B, TypeIntVector)
CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [50, 30, 10]), &
        "Get: negative stride, IntVector_ result")

! Test 5: array result
arr = Get(obj, 2_I4B, 4_I4B, 1_I4B, 1_I4B)
CALL OK(ALL(arr .EQ. [20, 30, 40]), "Get: triplet, array result")
arr = Get(obj, 5_I4B, 1_I4B, -2_I4B, 1_I4B)
CALL OK(ALL(arr .EQ. [50, 30, 10]), "Get: negative stride, array result")

! Test 6: a stride larger than the range gives one entry
arr = Get(obj, 2_I4B, 3_I4B, 5_I4B, 1_I4B)
CALL OK(ALL(arr .EQ. [20]), "Get: stride larger than the range")

END PROGRAM main
