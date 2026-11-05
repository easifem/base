!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that writes selected positions to an integer array

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Get_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj
INTEGER(I4B) :: arr(5), exact(2), tsize

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! Test 1: a larger array; order is kept, entries after tsize are not changed
arr = -1
CALL Get_(obj=obj, indx=[4_I4B, 1_I4B, 4_I4B], ans=arr, tsize=tsize)
CALL OK(tsize .EQ. 3, "Get_: indx, array, larger ans, tsize")
CALL OK(ALL(arr(1:3) .EQ. [40, 10, 40]), &
        "Get_: indx, array, larger ans, values")
CALL OK(ALL(arr(4:5) .EQ. -1), "Get_: indx, array, larger ans, rest is kept")

! Test 2: an array of the exact size
CALL Get_(obj=obj, indx=[2_I4B, 3_I4B], ans=exact, tsize=tsize)
CALL OK(tsize .EQ. 2, "Get_: indx, array, exact ans, tsize")
CALL OK(ALL(exact .EQ. [20, 30]), "Get_: indx, array, exact ans, values")

! Test 3: no positions writes nothing
BLOCK
  INTEGER(I4B) :: noindx(0)
  arr = -1
  CALL Get_(obj=obj, indx=noindx, ans=arr, tsize=tsize)
  CALL OK(tsize .EQ. 0, "Get_: empty indx, array, tsize")
  CALL OK(ALL(arr .EQ. -1), "Get_: empty indx, array, ans is kept")
END BLOCK

END PROGRAM main
