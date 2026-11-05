!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that writes one triplet of each item to an integer
! array

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: vec(2)
INTEGER(I4B) :: arr(6), exact(2), tsize

vec(1) = [1_I4B, 2_I4B, 3_I4B]
vec(2) = [4_I4B, 5_I4B, 6_I4B]

! Test 1: a larger array, entries after tsize are not changed
arr = -1
CALL Get_(vec, 1_I4B, 3_I4B, 2_I4B, arr, tsize)
CALL OK(tsize .EQ. 4, "Get_: vector obj, triplet, array, tsize")
CALL OK(ALL(arr(1:4) .EQ. [1, 3, 4, 6]), &
        "Get_: vector obj, triplet, array, values")
CALL OK(ALL(arr(5:6) .EQ. -1), &
        "Get_: vector obj, triplet, array, rest is kept")

! Test 2: negative stride walks backwards
CALL Get_(vec, 3_I4B, 2_I4B, -1_I4B, arr, tsize)
CALL OK(tsize .EQ. 4, "Get_: vector obj, array, negative stride, tsize")
CALL OK(ALL(arr(1:4) .EQ. [3, 2, 6, 5]), &
        "Get_: vector obj, array, negative stride, values")

! Test 3: an array of the exact size
CALL Get_(vec, 1_I4B, 1_I4B, 1_I4B, exact, tsize)
CALL OK(tsize .EQ. 2, "Get_: vector obj, triplet, exact ans, tsize")
CALL OK(ALL(exact .EQ. [1, 4]), &
        "Get_: vector obj, triplet, exact ans, values")

! Test 4: a triplet that selects no entries writes nothing
arr = -1
CALL Get_(vec, 3_I4B, 2_I4B, 2_I4B, arr, tsize)
CALL OK(tsize .EQ. 0, "Get_: vector obj, array, 3:2:2, tsize")
CALL OK(ALL(arr .EQ. -1), "Get_: vector obj, array, 3:2:2, ans is kept")

END PROGRAM main
