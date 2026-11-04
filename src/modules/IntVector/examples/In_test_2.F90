!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing a .IN. obj for an integer and an IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: OPERATOR(.IN.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

! Test 1: empty IntVector_ contains nothing
CALL OK(.NOT. (1_I4B .IN. obj), ".IN.: 1 not in empty obj")

obj = [1_I4B, 2_I4B, 3_I4B]

! Test 2: present entries
CALL OK(1_I4B .IN. obj, ".IN.: 1 in [1, 2, 3]")
CALL OK(3_I4B .IN. obj, ".IN.: 3 in [1, 2, 3]")

! Test 3: absent entries
CALL OK(.NOT. (4_I4B .IN. obj), ".IN.: 4 not in [1, 2, 3]")
CALL OK(.NOT. (-1_I4B .IN. obj), ".IN.: -1 not in [1, 2, 3]")

! Test 4: entries beyond tsize are not searched
obj%tsize = 2
CALL OK(.NOT. (3_I4B .IN. obj), ".IN.: 3 beyond tsize")
CALL OK(2_I4B .IN. obj, ".IN.: 2 within tsize")

END PROGRAM main
