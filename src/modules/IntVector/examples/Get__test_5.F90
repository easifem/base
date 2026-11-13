!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ for the same positions of each item of a vector

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

! Test 1: ans is allocated when needed; order is kept
CALL Get_(obj=vec, indx=[3_I4B, 1_I4B], ans=ans)
CALL OK(Size(ans) .EQ. 4, "Get_: vector obj, indx, new ans, Size")
CALL OK(ALL(ans%val(1:4) .EQ. [3, 1, 6, 4]), &
        "Get_: vector obj, indx, new ans, values")

! Test 2: a larger ans keeps its capacity
CALL Get_(obj=vec, indx=[2_I4B], ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: vector obj, indx, larger ans, Size")
CALL OK(Capacity(ans) .EQ. 4, "Get_: vector obj, indx, capacity is kept")
CALL OK(ALL(ans%val(1:2) .EQ. [2, 5]), &
        "Get_: vector obj, indx, larger ans, values")

! Test 3: a smaller ans grows
CALL Get_(obj=vec, indx=[1_I4B, 2_I4B, 3_I4B], ans=tiny)
CALL OK(Size(tiny) .EQ. 6, "Get_: vector obj, indx, smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 6, "Get_: vector obj, indx, capacity grows")
CALL OK(ALL(tiny%val(1:6) .EQ. [1, 2, 3, 4, 5, 6]), &
        "Get_: vector obj, indx, smaller ans, values")

! Test 4: no positions gives an empty result
BLOCK
  INTEGER(I4B) :: noindx(0)
  CALL Get_(obj=vec, indx=noindx, ans=ans)
  CALL OK(Size(ans) .EQ. 0, "Get_: vector obj, empty indx, Size")
  CALL OK(Capacity(ans) .EQ. 4, "Get_: vector obj, empty indx, capacity")
END BLOCK

END PROGRAM main
