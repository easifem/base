!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a logical vector

PROGRAM main
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT), ALLOCATABLE :: a(:)

! not allocated: allocate and set to .FALSE.
CALL Reallocate(a, 4_I4B)
CALL OK(ALLOCATED(a), "allocated")
CALL OK(SIZE(a) .EQ. 4_I4B, "size is 4")
CALL OK(.NOT. ANY(a), "all entries are .FALSE.")

! same size: the old values are not kept
a = .TRUE.
CALL Reallocate(a, 4_I4B)
CALL OK(SIZE(a) .EQ. 4_I4B, "same size is kept")
CALL OK(.NOT. ANY(a), "old values are reset")

! isExpand: a larger vector is kept, only a(1:row) is reset
a = .TRUE.
CALL Reallocate(a, 2_I4B, isExpand=.TRUE.)
CALL OK(SIZE(a) .EQ. 4_I4B, "isExpand keeps the larger size")
CALL OK(.NOT. ANY(a(1:2)), "a(1:2) is reset")
CALL OK(ALL(a(3:4)), "a(3:4) is not touched")

! without isExpand the size is changed to row
CALL Reallocate(a, 6_I4B)
CALL OK(SIZE(a) .EQ. 6_I4B, "size is 6")

END PROGRAM main
