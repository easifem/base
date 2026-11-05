!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Get_ with an index triplet on one IntVector_

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

obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B]
tiny = [7_I4B]

! Test 1: ans is allocated when needed
CALL Get_(obj, 1_I4B, 5_I4B, 2_I4B, ans)
CALL OK(Size(ans) .EQ. 3, "Get_: triplet, ans not allocated, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [10, 30, 50]), &
        "Get_: triplet, ans not allocated, values")

! Test 2: iend and stride default to Size(obj) and 1
CALL Get_(obj, istart=3_I4B, ans=ans)
CALL OK(Size(ans) .EQ. 3, "Get_: default iend and stride, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [30, 40, 50]), &
        "Get_: default iend and stride, values")

! Test 3: stride alone
CALL Get_(obj, istart=2_I4B, stride=2_I4B, ans=ans)
CALL OK(Size(ans) .EQ. 2, "Get_: default iend, Size")
CALL OK(ALL(ans%val(1:2) .EQ. [20, 40]), "Get_: default iend, values")

! Test 4: negative stride walks backwards
CALL Get_(obj, 5_I4B, 1_I4B, -2_I4B, ans)
CALL OK(Size(ans) .EQ. 3, "Get_: negative stride, Size")
CALL OK(ALL(ans%val(1:3) .EQ. [50, 30, 10]), &
        "Get_: negative stride, values")

! Test 5: a stride larger than the range gives one entry
CALL Get_(obj, 2_I4B, 3_I4B, 5_I4B, ans)
CALL OK(Size(ans) .EQ. 1, "Get_: stride larger than the range, Size")
CALL OK(ans%val(1) .EQ. 20, "Get_: stride larger than the range, value")
CALL OK(Capacity(ans) .EQ. 3, "Get_: larger ans, capacity is kept")

! Test 6: a smaller ans grows
CALL Get_(obj, 1_I4B, 5_I4B, 1_I4B, tiny)
CALL OK(Size(tiny) .EQ. 5, "Get_: smaller ans, Size")
CALL OK(Capacity(tiny) .EQ. 5, "Get_: smaller ans, capacity grows")
CALL OK(ALL(tiny%val(1:5) .EQ. [10, 20, 30, 40, 50]), &
        "Get_: smaller ans, values")

! Test 7: an unallocated obj gives zeros, old values of ans are cleared
CALL Get_(empty, 1_I4B, 2_I4B, 1_I4B, tiny)
CALL OK(Size(tiny) .EQ. 2, "Get_: unallocated obj, Size")
CALL OK(ALL(tiny%val(1:2) .EQ. 0), "Get_: unallocated obj, zeros")

! Test 8: a triplet that selects no entries gives an empty result
CALL Get_(obj, 3_I4B, 2_I4B, 2_I4B, ans)
CALL OK(Size(ans) .EQ. 0, "Get_: 3:2:2, Size")
CALL OK(Capacity(ans) .EQ. 3, "Get_: 3:2:2, capacity is kept")

END PROGRAM main
