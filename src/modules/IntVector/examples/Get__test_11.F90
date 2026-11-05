!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that writes the same positions of each item to an
! integer array

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

! Test 1: a larger array; order is kept, entries after tsize are not changed
arr = -1
CALL Get_(obj=vec, indx=[3_I4B, 1_I4B], ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 4, "Get_: vector obj, indx, array, tsize")
CALL OK(ALL(arr(1:4) .EQ. [3, 1, 6, 4]), &
        "Get_: vector obj, indx, array, values")
CALL OK(ALL(arr(5:6) .EQ. -1), "Get_: vector obj, indx, array, rest is kept")

! Test 2: an array of the exact size
CALL Get_(obj=vec, indx=[2_I4B], ans=exact, tsize=tsize)
CALL OK(tsize .EQ. 2, "Get_: vector obj, indx, exact ans, tsize")
CALL OK(ALL(exact .EQ. [2, 5]), "Get_: vector obj, indx, exact ans, values")

! Test 3: no positions writes nothing
BLOCK
  INTEGER(I4B) :: noindx(0)
  arr = -1
  CALL Get_(obj=vec, indx=noindx, ans=arr, tsize=tsize)
  CALL OK(tsize .EQ. 0, "Get_: vector obj, empty indx, array, tsize")
  CALL OK(ALL(arr .EQ. -1), "Get_: vector obj, empty indx, ans is kept")
END BLOCK

END PROGRAM main
