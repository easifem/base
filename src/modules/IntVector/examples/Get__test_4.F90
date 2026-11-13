!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ that joins a vector of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Capacity
USE IntVector_Method, ONLY: Get_
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: vec(3), none(2), ans, tiny, fresh

vec(1) = [1_I4B, 2_I4B]
vec(3) = [3_I4B, 4_I4B, 5_I4B]
tiny = [7_I4B]

! Test 1: ans is allocated when needed, an empty item is skipped
CALL Get_(obj=vec, ans=ans)
CALL OK(Size(ans) .EQ. 5, "Get_: join, ans not allocated, Size")
CALL OK(ALL(ans%val(1:5) .EQ. [1, 2, 3, 4, 5]), &
        "Get_: join, ans not allocated, values")

! Test 2: a larger ans keeps its capacity
CALL Get_(obj=vec(1:1), ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: join, larger ans, Size")
CALL OK(Capacity(ans) .EQ. 5, "Get_: join, larger ans, capacity is kept")
CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "Get_: join, larger ans, values")

! Test 3: a smaller ans grows
CALL Get_(obj=vec, ans=tiny)
CALL OK(Size(tiny) .EQ. 5, "Get_: join, smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 5, "Get_: join, smaller ans, capacity grows")
CALL OK(ALL(tiny%val(1:5) .EQ. [1, 2, 3, 4, 5]), &
        "Get_: join, smaller ans, values")

! Test 4: only empty items give an empty result
CALL Get_(obj=none, ans=ans)
CALL OK(Size(ans) .EQ. 0, "Get_: join, empty items, Size")
CALL OK(Capacity(ans) .EQ. 5, "Get_: join, empty items, capacity is kept")

! Test 5: entries beyond tsize are not joined
vec(3)%tsize = 1
CALL Get_(obj=vec, ans=fresh)
CALL OK(Size(fresh) .EQ. 3, "Get_: join, spare capacity, Size")
CALL OK(ALL(fresh%val(1:3) .EQ. [1, 2, 3]), &
        "Get_: join, spare capacity, values")

END PROGRAM main
