!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that joins a vector of IntVector_ to an integer array

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: vec(3), none(2)
INTEGER(I4B) :: arr(7), exact(5), tsize

vec(1) = [1_I4B, 2_I4B]
vec(3) = [3_I4B, 4_I4B, 5_I4B]

! Test 1: a larger array, an empty item is skipped
arr = -1
CALL Get_(obj=vec, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 5, "Get_: join, array, larger ans, tsize")
CALL OK(ALL(arr(1:5) .EQ. [1, 2, 3, 4, 5]), &
        "Get_: join, array, larger ans, values")
CALL OK(ALL(arr(6:7) .EQ. -1), "Get_: join, array, larger ans, rest is kept")

! Test 2: an array of the exact size
CALL Get_(obj=vec, ans=exact, tsize=tsize)
CALL OK(tsize .EQ. 5, "Get_: join, array, exact ans, tsize")
CALL OK(ALL(exact .EQ. [1, 2, 3, 4, 5]), &
        "Get_: join, array, exact ans, values")

! Test 3: only empty items write nothing
arr = -1
CALL Get_(obj=none, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 0, "Get_: join, array, empty items, tsize")
CALL OK(ALL(arr .EQ. -1), "Get_: join, array, empty items, ans is kept")

! Test 4: entries beyond tsize are not joined
arr = -1
vec(3)%tsize = 1
CALL Get_(obj=vec, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 3, "Get_: join, array, spare capacity, tsize")
CALL OK(ALL(arr(1:3) .EQ. [1, 2, 3]), &
        "Get_: join, array, spare capacity, values")

END PROGRAM main
