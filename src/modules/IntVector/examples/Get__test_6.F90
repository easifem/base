!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ with one index triplet on each item of a vector

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Get_
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: vec(2), ans, tiny

vec(1) = [1_I4B, 2_I4B, 3_I4B]
vec(2) = [4_I4B, 5_I4B, 6_I4B]
tiny = [7_I4B]

! Test 1: ans is allocated when needed
CALL Get_(vec, 1_I4B, 3_I4B, 2_I4B, ans)
CALL OK(Size(ans) .EQ. 4, "Get_: vector obj, triplet, new ans, Size")
CALL OK(ALL(ans%val(1:4) .EQ. [1, 3, 4, 6]), &
        "Get_: vector obj, triplet, new ans, values")

! Test 2: negative stride walks backwards
CALL Get_(vec, 3_I4B, 2_I4B, -1_I4B, ans)
CALL OK(Size(ans) .EQ. 4, "Get_: vector obj, negative stride, Size")
CALL OK(ALL(ans%val(1:4) .EQ. [3, 2, 6, 5]), &
        "Get_: vector obj, negative stride, values")

! Test 3: a larger ans keeps its capacity
CALL Get_(vec, 1_I4B, 1_I4B, 1_I4B, ans)
CALL OK(Size(ans) .EQ. 2, "Get_: vector obj, triplet, larger ans, Size")
CALL OK(Capacity(ans) .EQ. 4, "Get_: vector obj, triplet, capacity is kept")
CALL OK(ALL(ans%val(1:2) .EQ. [1, 4]), &
        "Get_: vector obj, triplet, larger ans, values")

! Test 4: a smaller ans grows
CALL Get_(vec, 1_I4B, 3_I4B, 1_I4B, tiny)
CALL OK(Size(tiny) .EQ. 6, "Get_: vector obj, triplet, smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 6, "Get_: vector obj, triplet, capacity grows")
CALL OK(ALL(tiny%val(1:6) .EQ. [1, 2, 3, 4, 5, 6]), &
        "Get_: vector obj, triplet, smaller ans, values")

! Test 5: a triplet that selects no entries gives an empty result
CALL Get_(vec, 3_I4B, 2_I4B, 2_I4B, ans)
CALL OK(Size(ans) .EQ. 0, "Get_: vector obj, 3:2:2, Size")
CALL OK(Capacity(ans) .EQ. 4, "Get_: vector obj, 3:2:2, capacity is kept")

END PROGRAM main
