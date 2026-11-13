!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: a .isin. b for integer vectors (entrywise test)

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IntegerUtility, ONLY: OPERATOR(.isin.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(4), b(2)
LOGICAL(LGT) :: ans4(4), ans2(2)

a = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
b = [12_I4B, 9_I4B]

! ans(i) is true if a(i) is in b
ans4 = a .isin. b
CALL OK(ALL(ans4 .EQV. [.TRUE., .FALSE., .FALSE., .TRUE.]), &
        "a .isin. b")

ans2 = b .isin. a
CALL OK(ALL(ans2), "b .isin. a")

! the size of the result is the size of a
CALL OK(SIZE(a .isin. b) .EQ. 4_I4B, "size of the result")

! no common entry
CALL OK(.NOT. ANY([1_I4B, 2_I4B] .isin. a), "no common entry")

END PROGRAM main
