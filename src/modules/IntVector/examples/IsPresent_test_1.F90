!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing IsPresent for a scalar and a vector of values

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: IsPresent
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

! Test 1: empty IntVector_ contains nothing
CALL OK(.NOT. IsPresent(obj, 1_I4B), "IsPresent: empty obj, scalar")
CALL OK(.NOT. ANY(IsPresent(obj, [1_I4B, 2_I4B])), &
        "IsPresent: empty obj, vector")

obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]

! Test 2: scalar value
CALL OK(IsPresent(obj, 7_I4B), "IsPresent: 7 in [5, 7, 5, 9]")
CALL OK(IsPresent(obj, 5_I4B), "IsPresent: repeated 5")
CALL OK(.NOT. IsPresent(obj, 6_I4B), "IsPresent: 6 not present")

! Test 3: vector of values, one result per value
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: ans(:)
  ans = IsPresent(obj, [9_I4B, 1_I4B, 5_I4B, 5_I4B])
  CALL OK(SIZE(ans) .EQ. 4, "IsPresent: SIZE(ans) = SIZE(value)")
  CALL OK(ALL(ans .EQV. [.TRUE., .FALSE., .TRUE., .TRUE.]), &
          "IsPresent: [9, 1, 5, 5]")
END BLOCK

! Test 4: empty vector of values gives an empty result
BLOCK
  INTEGER(I4B) :: novalue(0)
  CALL OK(SIZE(IsPresent(obj, novalue)) .EQ. 0, &
          "IsPresent: empty value")
END BLOCK

! Test 5: entries beyond tsize are not searched
obj%tsize = 3
CALL OK(.NOT. IsPresent(obj, 9_I4B), "IsPresent: 9 beyond tsize")
CALL OK(ALL(IsPresent(obj, [5_I4B, 9_I4B]) .EQV. [.TRUE., .FALSE.]), &
        "IsPresent: [5, 9] with tsize = 3")

END PROGRAM main
