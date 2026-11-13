!> author: Shion Shimizu
! date: 2026-11-13
! summary: Reallocate a logical vector with the size given as a vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: not allocated, the size is s(1)
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: a(:)

  CALL Reallocate(a, [5_I4B])
  CALL OK(SIZE(a) .EQ. 5_I4B, "size is s(1)")
  CALL OK(.NOT. ANY(a), "all entries are .FALSE.")
END BLOCK

! Test 2: same size, the old values are reset
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: a(:)

  CALL Reallocate(a, [5_I4B])
  a = .TRUE.
  CALL Reallocate(a, [5_I4B])
  CALL OK(.NOT. ANY(a), "old values are reset")
END BLOCK

! Test 3: isExpand keeps a larger vector, only a(1:s(1)) is reset
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: a(:)

  CALL Reallocate(a, [6_I4B])
  a = .TRUE.
  CALL Reallocate(a, [2_I4B], isExpand=.TRUE.)
  CALL OK(SIZE(a) .EQ. 6_I4B, "isExpand keeps the larger size")
  CALL OK(.NOT. ANY(a(1:2)), "a(1:2) is reset")
  CALL OK(ALL(a(3:6)), "a(3:6) is not touched")
END BLOCK

! Test 4: isExpand grows to s(1) * expandFactor, all .FALSE.
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: a(:)

  CALL Reallocate(a, [2_I4B])
  CALL Reallocate(a, [4_I4B], isExpand=.TRUE., expandFactor=3_I4B)
  CALL OK(SIZE(a) .EQ. 12_I4B, "size is 4 * 3")
  CALL OK(.NOT. ANY(a), "new vector is .FALSE. everywhere")
END BLOCK

END PROGRAM main
