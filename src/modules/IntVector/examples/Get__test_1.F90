!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing Get_ for the whole of one IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Get_
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj, small, empty, ans, tiny, fresh

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
small = [1_I4B, 2_I4B]
tiny = [7_I4B]

! Test 1: ans is allocated when needed
CALL Get_(obj=obj, ans=ans)
CALL OK(Size(ans) .EQ. 4, "Get_: ans not allocated, Size")
CALL OK(ALL(ans%val(1:4) .EQ. [10, 20, 30, 40]), &
        "Get_: ans not allocated, values")

! Test 2: a larger ans keeps its capacity
CALL Get_(obj=small, ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: larger ans, Size")
CALL OK(Capacity(ans) .EQ. 4, "Get_: larger ans, capacity is kept")
CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "Get_: larger ans, values")

! Test 3: a smaller ans grows
CALL Get_(obj=obj, ans=tiny)
CALL OK(Size(tiny) .EQ. 4, "Get_: smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 4, "Get_: smaller ans, capacity grows")
CALL OK(ALL(tiny%val(1:4) .EQ. [10, 20, 30, 40]), &
        "Get_: smaller ans, values")

! Test 4: an empty obj gives an empty result
CALL Get_(obj=empty, ans=ans)
CALL OK(Size(ans) .EQ. 0, "Get_: empty obj, Size")
CALL OK(Capacity(ans) .EQ. 4, "Get_: empty obj, capacity is kept")

! Test 5: entries beyond tsize are not returned
obj%tsize = 2
CALL Get_(obj=obj, ans=fresh)
CALL OK(Size(fresh) .EQ. 2, "Get_: spare capacity, Size")
CALL OK(Capacity(fresh) .EQ. 2, "Get_: result capacity = tsize")
CALL OK(ALL(fresh%val(1:2) .EQ. [10, 20]), "Get_: spare capacity, values")

END PROGRAM main
