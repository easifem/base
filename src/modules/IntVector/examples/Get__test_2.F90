!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ for selected positions of one IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Get_
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, empty, ans, tiny

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
tiny = [7_I4B]

! Test 1: ans is allocated when needed; order is kept
CALL Get_(obj=obj, indx=[4_I4B, 1_I4B, 4_I4B], ans=ans)
CALL OK(Size(ans) .EQ. 3, "Get_: indx, ans not allocated, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [40, 10, 40]), &
        "Get_: indx, ans not allocated, values")

! Test 2: a larger ans keeps its capacity
CALL Get_(obj=obj, indx=[2_I4B, 3_I4B], ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: indx, larger ans, Size")
CALL OK(Capacity(ans) .EQ. 3, "Get_: indx, larger ans, capacity is kept")
CALL OK(ALL(ans%val(1:2) .EQ. [20, 30]), "Get_: indx, larger ans, values")

! Test 3: a smaller ans grows
CALL Get_(obj=obj, indx=[3_I4B, 2_I4B, 1_I4B], ans=tiny)
CALL OK(Size(tiny) .EQ. 3, "Get_: indx, smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 3, "Get_: indx, smaller ans, capacity grows")
CALL OK(ALL(tiny%val(1:3) .EQ. [30, 20, 10]), &
        "Get_: indx, smaller ans, values")

! Test 4: an unallocated obj gives zeros, old values of ans are cleared
CALL Get_(obj=empty, indx=[1_I4B, 2_I4B], ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: indx, unallocated obj, Size")
CALL OK(ALL(ans%val(1:2) .EQ. 0), "Get_: indx, unallocated obj, zeros")

! Test 5: no positions gives an empty result
BLOCK
  INTEGER(I4B) :: noindx(0)
  CALL Get_(obj=obj, indx=noindx, ans=ans)
  CALL OK(Size(ans) .EQ. 0, "Get_: empty indx, Size")
  CALL OK(Capacity(ans) .EQ. 3, "Get_: empty indx, capacity is kept")
END BLOCK

END PROGRAM main
