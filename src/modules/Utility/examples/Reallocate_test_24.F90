!> author: Shion Shimizu
! date: 2026-11-13
! summary: Reallocate a logical rank-2 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: not allocated, allocate (row, col) and set to .FALSE.
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :)

  CALL Reallocate(m, 3_I4B, 2_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [3_I4B, 2_I4B]), "shape is (3, 2)")
  CALL OK(.NOT. ANY(m), "all entries are .FALSE.")
END BLOCK

! Test 2: same shape resets, another shape reallocates
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :)

  CALL Reallocate(m, 3_I4B, 2_I4B)
  m = .TRUE.
  CALL Reallocate(m, 3_I4B, 2_I4B)
  CALL OK(.NOT. ANY(m), "same shape: old values are reset")
  CALL Reallocate(m, 1_I4B, 4_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [1_I4B, 4_I4B]), "new shape is (1, 4)")
END BLOCK

! Test 3: isExpand keeps a larger array, only the block is reset
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :)

  CALL Reallocate(m, 4_I4B, 3_I4B)
  m = .TRUE.
  CALL Reallocate(m, 2_I4B, 2_I4B, isExpand=.TRUE.)
  CALL OK(ALL(SHAPE(m) .EQ. [4_I4B, 3_I4B]), "isExpand keeps (4, 3)")
  CALL OK(.NOT. ANY(m(1:2, 1:2)), "m(1:2, 1:2) is reset")
  CALL OK(ALL(m(3:4, :)) .AND. ALL(m(:, 3)), "other entries kept")
END BLOCK

! Test 4: isExpand grows a too small extent by expandFactor
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :)

  CALL Reallocate(m, 2_I4B, 5_I4B)
  CALL Reallocate(m, 3_I4B, 2_I4B, isExpand=.TRUE., expandFactor=2_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [6_I4B, 5_I4B]), "shape is (3 * 2, 5)")
  CALL OK(.NOT. ANY(m), "new array is .FALSE. everywhere")
END BLOCK

! Test 5: shape given as a vector, and a zero extent
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :)

  CALL Reallocate(m, [2_I4B, 3_I4B])
  CALL OK(ALL(SHAPE(m) .EQ. [2_I4B, 3_I4B]), "shape from s")
  CALL Reallocate(m, 0_I4B, 3_I4B)
  CALL OK(SIZE(m) .EQ. 0_I4B, "size is 0")
END BLOCK

END PROGRAM main
