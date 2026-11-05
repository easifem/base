!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing H_CONCAT for arrays and IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: H_CONCAT
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj1, obj2, ans
INTEGER(I4B) :: empty(0)

obj1 = [1_I4B, 2_I4B]
obj2 = [3_I4B, 4_I4B, 5_I4B]

! Test 1: two integer arrays
BLOCK
  INTEGER(I4B), ALLOCATABLE :: arr(:)
  arr = H_CONCAT([1_I4B, 2_I4B], [3_I4B])
  CALL OK(SIZE(arr) .EQ. 3, "H_CONCAT: arrays, size")
  CALL OK(ALL(arr .EQ. [1, 2, 3]), "H_CONCAT: arrays, values")
  arr = H_CONCAT(empty, [3_I4B])
  CALL OK(ALL(arr .EQ. [3]), "H_CONCAT: arrays, empty first")
  arr = H_CONCAT([3_I4B], empty)
  CALL OK(ALL(arr .EQ. [3]), "H_CONCAT: arrays, empty second")
END BLOCK

! Test 2: two IntVector_
ans = H_CONCAT(obj1, obj2)
CALL OK(Size(ans) .EQ. 5, "H_CONCAT: obj obj, size")
CALL OK(ALL(ans%val(1:5) .EQ. [1, 2, 3, 4, 5]), "H_CONCAT: obj obj, val")
CALL OK(ans%capacity .EQ. 5, "H_CONCAT: obj obj, capacity")

! Test 3: array then IntVector_
ans = H_CONCAT([9_I4B], obj2)
CALL OK(Size(ans) .EQ. 4, "H_CONCAT: vec obj, size")
CALL OK(ALL(ans%val(1:4) .EQ. [9, 3, 4, 5]), "H_CONCAT: vec obj, val")

! Test 4: IntVector_ then array
ans = H_CONCAT(obj1, [9_I4B])
CALL OK(Size(ans) .EQ. 3, "H_CONCAT: obj vec, size")
CALL OK(ALL(ans%val(1:3) .EQ. [1, 2, 9]), "H_CONCAT: obj vec, val")

! Test 5: spare capacity of the inputs is ignored
BLOCK
  TYPE(IntVector_) :: spare
  CALL Append(spare, 7_I4B)
  CALL OK(spare%capacity .GT. Size(spare), "H_CONCAT: input has spare")
  ans = H_CONCAT(spare, obj1)
  CALL OK(Size(ans) .EQ. 3, "H_CONCAT: spare first, size")
  CALL OK(ALL(ans%val(1:3) .EQ. [7, 1, 2]), "H_CONCAT: spare 1st, val")
  ans = H_CONCAT(obj1, spare)
  CALL OK(ALL(ans%val(1:3) .EQ. [1, 2, 7]), "H_CONCAT: spare 2nd, val")
  ans = H_CONCAT([9_I4B], spare)
  CALL OK(ALL(ans%val(1:2) .EQ. [9, 7]), "H_CONCAT: spare, vec obj")
  ans = H_CONCAT(spare, [9_I4B])
  CALL OK(ALL(ans%val(1:2) .EQ. [7, 9]), "H_CONCAT: spare, obj vec")
END BLOCK

! Test 6: empty, never allocated IntVector_
BLOCK
  TYPE(IntVector_) :: none1, none2
  ans = H_CONCAT(none1, none2)
  CALL OK(Size(ans) .EQ. 0, "H_CONCAT: both empty, size")
  ans = H_CONCAT(none1, obj1)
  CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "H_CONCAT: empty first, val")
  ans = H_CONCAT(obj1, none1)
  CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "H_CONCAT: empty second, val")
  ans = H_CONCAT(empty, none1)
  CALL OK(Size(ans) .EQ. 0, "H_CONCAT: empty vec obj, size")
  ans = H_CONCAT(none1, empty)
  CALL OK(Size(ans) .EQ. 0, "H_CONCAT: empty obj vec, size")
END BLOCK

END PROGRAM main
