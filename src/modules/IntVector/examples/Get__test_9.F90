!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that writes a triplet to an integer array

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj
INTEGER(I4B) :: arr(5), exact(3), tsize

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B]

! Test 1: a larger array, entries after tsize are not changed
arr = -1
CALL Get_(obj, 1_I4B, 5_I4B, 2_I4B, arr, tsize)
CALL OK(tsize .EQ. 3, "Get_: triplet, array, larger ans, tsize")
CALL OK(ALL(arr(1:3) .EQ. [10, 30, 50]), &
        "Get_: triplet, array, larger ans, values")
CALL OK(ALL(arr(4:5) .EQ. -1), &
        "Get_: triplet, array, larger ans, rest is kept")

! Test 2: an array of the exact size, negative stride walks backwards
CALL Get_(obj, 5_I4B, 1_I4B, -2_I4B, exact, tsize)
CALL OK(tsize .EQ. 3, "Get_: array, negative stride, tsize")
CALL OK(ALL(exact .EQ. [50, 30, 10]), "Get_: array, negative stride, values")

! Test 3: a stride larger than the range gives one entry
arr = -1
CALL Get_(obj, 2_I4B, 3_I4B, 5_I4B, arr, tsize)
CALL OK(tsize .EQ. 1, "Get_: array, stride larger than range, tsize")
CALL OK(arr(1) .EQ. 20, "Get_: array, stride larger than range, value")

! Test 4: a triplet that selects no entries writes nothing
arr = -1
CALL Get_(obj, 3_I4B, 2_I4B, 2_I4B, arr, tsize)
CALL OK(tsize .EQ. 0, "Get_: array, 3:2:2, tsize")
CALL OK(ALL(arr .EQ. -1), "Get_: array, 3:2:2, ans is kept")

END PROGRAM main
