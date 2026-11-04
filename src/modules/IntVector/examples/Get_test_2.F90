!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get for selected positions of one IntVector_

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

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! Test 1: vector of positions, IntVector_ result; order is kept
ans = Get(obj, [4_I4B, 1_I4B, 4_I4B], TypeIntVector)
CALL OK(Size(ans) .EQ. 3, "Get: indx, IntVector_ result, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [40, 10, 40]), &
        "Get: indx, IntVector_ result, values")

! Test 2: vector of positions, array result
arr = Get(obj, [2_I4B, 3_I4B], 1_I4B)
CALL OK(ALL(arr .EQ. [20, 30]), "Get: indx, array result")

! Test 3: no positions gives an empty result
BLOCK
  INTEGER(I4B) :: noindx(0)
  ans = Get(obj, noindx, TypeIntVector)
  CALL OK(Size(ans) .EQ. 0, "Get: empty indx, IntVector_ result")
  arr = Get(obj, noindx, 1_I4B)
  CALL OK(SIZE(arr) .EQ. 0, "Get: empty indx, array result")
END BLOCK

! Test 4: one position gives a scalar
CALL OK(Get(obj, 3_I4B, 1_I4B) .EQ. 30, "Get: scalar indx")

! Test 5: a scalar position beyond tsize gives zero
CALL OK(Get(obj, 5_I4B, 1_I4B) .EQ. 0, "Get: scalar indx > tsize")

END PROGRAM main
