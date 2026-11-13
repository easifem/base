!> author: Vikas Sharma, Ph. D.
! date: 2026-11-04
! summary: Testing GetIndex for a scalar value that is absent
!
! GetIndex(obj, val(:)) gives 0 for an absent value. The scalar version
! should agree with it.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: GetIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]

! Test 1: an absent value gives 0
CALL OK(GetIndex(obj, 6_I4B) .EQ. 0, "GetIndex: 6 is absent")
CALL OK(GetIndex(obj, 100_I4B) .EQ. 0, "GetIndex: 100 is absent")

! Test 2: an entry beyond tsize is absent
obj%tsize = 3
CALL OK(GetIndex(obj, 9_I4B) .EQ. 0, "GetIndex: 9 beyond tsize")

END PROGRAM main
