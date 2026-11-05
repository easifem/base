!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that writes one IntVector_ to an integer array

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, empty
INTEGER(I4B) :: arr(6), exact(4), tsize

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! Test 1: a larger array, entries after tsize are not changed
arr = -1
CALL Get_(obj=obj, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 4, "Get_: array, larger ans, tsize")
CALL OK(ALL(arr(1:4) .EQ. [10, 20, 30, 40]), &
        "Get_: array, larger ans, values")
CALL OK(ALL(arr(5:6) .EQ. -1), "Get_: array, larger ans, rest is kept")

! Test 2: an array of the exact size
CALL Get_(obj=obj, ans=exact, tsize=tsize)
CALL OK(tsize .EQ. 4, "Get_: array, exact ans, tsize")
CALL OK(ALL(exact .EQ. [10, 20, 30, 40]), "Get_: array, exact ans, values")

! Test 3: an empty obj writes nothing
arr = -1
CALL Get_(obj=empty, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 0, "Get_: array, empty obj, tsize")
CALL OK(ALL(arr .EQ. -1), "Get_: array, empty obj, ans is kept")

! Test 4: entries beyond tsize are not returned
arr = -1
obj%tsize = 2
CALL Get_(obj=obj, ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 2, "Get_: array, spare capacity, tsize")
CALL OK(ALL(arr(1:2) .EQ. [10, 20]), "Get_: array, spare capacity, values")
CALL OK(ALL(arr(3:6) .EQ. -1), "Get_: array, spare capacity, rest is kept")

END PROGRAM main
