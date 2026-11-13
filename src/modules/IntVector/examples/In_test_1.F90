!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing obj1 .IN. obj2 for two IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: OPERATOR(.IN.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj1, obj2

obj2 = [9_I4B, 10_I4B, 11_I4B, 12_I4B]

! Test 1: every entry of obj1 is in obj2
obj1 = [12_I4B, 9_I4B]
CALL OK(obj1 .IN. obj2, ".IN.: [12, 9] in [9, 10, 11, 12]")

! Test 2: obj2 has entries that are not in obj1
CALL OK(.NOT. (obj2 .IN. obj1), ".IN.: [9, 10, 11, 12] not in [12, 9]")

! Test 3: one entry is missing
obj1 = [9_I4B, 13_I4B]
CALL OK(.NOT. (obj1 .IN. obj2), ".IN.: [9, 13] not in obj2")

! Test 4: a set is a subset of itself
CALL OK(obj2 .IN. obj2, ".IN.: obj2 in obj2")

! Test 5: only the active entries val(1:tsize) of obj2 are searched
obj1 = [12_I4B]
obj2%tsize = 3
CALL OK(.NOT. (obj1 .IN. obj2), ".IN.: 12 beyond tsize of obj2")

! Test 6: only the active entries val(1:tsize) of obj1 are checked
obj1 = [9_I4B, 13_I4B]
obj1%tsize = 1
CALL OK(obj1 .IN. obj2, ".IN.: 13 beyond tsize of obj1")

END PROGRAM main
