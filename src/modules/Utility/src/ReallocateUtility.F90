! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates vectors and arrays.
!
! Description:
!   This module contains the generic subroutine `Reallocate`. It makes
!   an allocatable array ready to hold a given shape and sets its
!   entries to zero (`.FALSE.` for logical). The old values are never
!   kept, so `Reallocate` is not a resize that copies data.
!
!   `Reallocate` is available for
!
!   - logical vectors,
!   - real (`REAL32`, `REAL64`) arrays of rank 1 to 7,
!   - integer arrays of rank 1 to 7 (`INT8` to `INT64` for rank 1 and
!     2, `INT32` and `INT64` for rank 3 to 7),
!   - up to six integer or real vectors in one call,
!   - a real vector with one or two integer vectors (sparse storage).
!
!   The shape is given either as separate sizes or as a vector `s`.
!   For vectors and for arrays of rank 2 and 3, the optional
!   `isExpand` keeps an array that is already large enough, and
!   `expandFactor` makes a new allocation larger than needed. For
!   the other overloads these two arguments are not used.

MODULE ReallocateUtility
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64

IMPLICIT NONE

PRIVATE

PUBLIC :: Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a logical vector.
!
! Description:
!   Makes `mat` ready to hold `row` entries and sets `mat(1:row)` to
!   `.FALSE.`. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated with size
!     `row * expandFactor`. `expandFactor` is used here even when
!     `isExpand` is absent.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when `SIZE(mat) /= row`, so that
!     `SIZE(mat) = row`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, it is kept when
!     `SIZE(mat) >= row`, and reallocated with size
!     `row * expandFactor` otherwise.
!   - Only `mat(1:row)` is set. Entries after `row` keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
! Arguments:
!   mat : LOGICAL(LGT), ALLOCATABLE, rank(1) - vector to reallocate
!   row : INTEGER(I4B) - required size, `row >= 0`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat) >= row`, with `mat(1:row) = .FALSE.`
!
! Formula:
!   $$n = \begin{cases}
!     \mathrm{row} \cdot f, & \text{mat is not allocated} \\
!     \mathrm{row}, & \text{isExpand is false} \\
!     n_{old}, & \text{isExpand}, n_{old} \ge \mathrm{row} \\
!     \mathrm{row} \cdot f, & \text{isExpand}, n_{old} < \mathrm{row}
!   \end{cases}$$
!
!   $$\mathrm{mat}_i = \text{false}, \; i = 1, \dots, \mathrm{row}$$
!
!   where $n = \mathrm{SIZE(mat)}$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT), ALLOCATABLE :: a(:)
!
! ! not allocated: allocate and set to .FALSE.
! CALL Reallocate(a, 4_I4B)
! CALL OK(ALLOCATED(a), "allocated")
! CALL OK(SIZE(a) .EQ. 4_I4B, "size is 4")
! CALL OK(.NOT. ANY(a), "all entries are .FALSE.")
!
! ! same size: the old values are not kept
! a = .TRUE.
! CALL Reallocate(a, 4_I4B)
! CALL OK(SIZE(a) .EQ. 4_I4B, "same size is kept")
! CALL OK(.NOT. ANY(a), "old values are reset")
!
! ! isExpand: a larger vector is kept, only a(1:row) is reset
! a = .TRUE.
! CALL Reallocate(a, 2_I4B, isExpand=.TRUE.)
! CALL OK(SIZE(a) .EQ. 4_I4B, "isExpand keeps the larger size")
! CALL OK(.NOT. ANY(a(1:2)), "a(1:2) is reset")
! CALL OK(ALL(a(3:4)), "a(3:4) is not touched")
!
! ! without isExpand the size is changed to row
! CALL Reallocate(a, 6_I4B)
! CALL OK(SIZE(a) .EQ. 6_I4B, "size is 6")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_logical(mat, row, isExpand, expandFactor)
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times row
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
    !! expand factor, used when isExpand is true.
  END SUBROUTINE Reallocate_logical
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real vector.
!
! Description:
!   Makes `mat` ready to hold `row` entries and sets `mat(1:row)` to
!   zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated with size
!     `row * expandFactor`. `expandFactor` is used here even when
!     `isExpand` is absent.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when `SIZE(mat) /= row`, so that
!     `SIZE(mat) = row`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, it is kept when
!     `SIZE(mat) >= row`, and reallocated with size
!     `row * expandFactor` otherwise.
!   - Only `mat(1:row)` is set. Entries after `row` keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The size can also be given as a vector `s`; then `row = s(1)`
!   and the other entries of `s` are not used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(1) - vector to reallocate
!   row : INTEGER(I4B) - required size, `row >= 0`
!   s : INTEGER(I4B), rank(1) - required size as `s(1)`, `SIZE(s) >= 1`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat) >= row`, with `mat(1:row) = 0`
!
! Formula:
!   $$n = \begin{cases}
!     \mathrm{row} \cdot f, & \text{mat is not allocated} \\
!     \mathrm{row}, & \text{isExpand is false} \\
!     n_{old}, & \text{isExpand}, n_{old} \ge \mathrm{row} \\
!     \mathrm{row} \cdot f, & \text{isExpand}, n_{old} < \mathrm{row}
!   \end{cases}$$
!
!   $$\mathrm{mat}_i = 0, \; i = 1, \dots, \mathrm{row}$$
!
!   where $n = \mathrm{SIZE(mat)}$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 10_I4B)
! CALL OK(SIZE(a) .EQ. 10_I4B, "size is 10")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! same size: the old values are not kept
! a = 1.0_DFP
! CALL Reallocate(a, 10_I4B)
! CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")
!
! ! without isExpand the vector can shrink
! CALL Reallocate(a, 5_I4B)
! CALL OK(SIZE(a) .EQ. 5_I4B, "size is 5")
!
! ! isExpand: grow when the vector is too small
! CALL Reallocate(a, 20_I4B, isExpand=.TRUE., expandFactor=1_I4B)
! CALL OK(SIZE(a) .EQ. 20_I4B, "isExpand grows to 20")
!
! ! isExpand: keep a larger vector, only a(1:row) is reset
! a = 1.0_DFP
! CALL Reallocate(a, 10_I4B, isExpand=.TRUE.)
! CALL OK(SIZE(a) .EQ. 20_I4B, "isExpand keeps size 20")
! CALL OK(ALL(a(1:10) .EQ. 0.0_DFP), "a(1:10) is reset")
! CALL OK(ALL(a(11:20) .EQ. 1.0_DFP), "a(11:20) is not touched")
!
! ! expandFactor: the new size is row * expandFactor
! CALL Reallocate(a, 30_I4B, isExpand=.TRUE., expandFactor=2_I4B)
! CALL OK(SIZE(a) .EQ. 60_I4B, "size is 30 * 2")
!
! ! the size can also be given as a vector
! CALL Reallocate(a, [4_I4B])
! CALL OK(SIZE(a) .EQ. 4_I4B, "size from s(1)")
!
! ! a vector of size zero
! CALL Reallocate(a, 0_I4B)
! CALL OK(ALLOCATED(a), "allocated with size 0")
! CALL OK(SIZE(a) .EQ. 0_I4B, "size is 0")
!
! END PROGRAM main
!```

INTERFACE Reallocate
 MODULE PURE SUBROUTINE Reallocate_Real64_R1(mat, row, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1

  MODULE PURE SUBROUTINE Reallocate_Real64_R1b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1b

 MODULE PURE SUBROUTINE Reallocate_Real32_R1(mat, row, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1

  MODULE PURE SUBROUTINE Reallocate_Real32_R1b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-2 array.
!
! Description:
!   Makes `mat` ready to hold an array of shape (row, col) and sets
!   the leading block `mat(1:row, ...)` to zero. The old values
!   are not kept.
!
!   - If `mat` is not allocated, it is allocated with shape (row, col).
!     `expandFactor` is not used in this case.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when its shape differs, so that
!     `SHAPE(mat) = [row, col]`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, every
!     dimension that is too small grows to `n_k * expandFactor`, and
!     the other dimensions keep their size. If any dimension grows,
!     `mat` is reallocated.
!   - Only the leading block is set. Other entries keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The shape can also be given as a vector `s`; then the first 2
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(2) - array to reallocate
!   row, col : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 2`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat, k) >= n_k` for every k, with the leading
!     block set to zero
!
! Formula:
!   $$s_k = \begin{cases}
!     n_k, & \text{mat is not allocated or isExpand is false} \\
!     s_k^{old}, & \text{isExpand}, s_k^{old} \ge n_k \\
!     n_k f, & \text{isExpand}, s_k^{old} < n_k
!   \end{cases}$$
!
!   where $s_k = \mathrm{SIZE(mat, k)}$, $n_k$ is the requested size of
!   dimension $k$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 10_I4B, 4_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [10_I4B, 4_I4B]), "shape is (10, 4)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! isExpand: grow the rows
! CALL Reallocate(a, 20_I4B, 4_I4B, isExpand=.TRUE., expandFactor=1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [20_I4B, 4_I4B]), "shape is (20, 4)")
!
! ! isExpand: a larger matrix is kept
! CALL Reallocate(a, 10_I4B, 4_I4B, isExpand=.TRUE., expandFactor=1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [20_I4B, 4_I4B]), "shape is still (20, 4)")
!
! ! expandFactor: only a dimension that is too small grows
! CALL Reallocate(a, 30_I4B, 4_I4B, isExpand=.TRUE., expandFactor=2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 4_I4B]), "shape is (60, 4)")
!
! CALL Reallocate(a, 30_I4B, 8_I4B, isExpand=.TRUE., expandFactor=2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 16_I4B]), "shape is (60, 16)")
!
! ! isExpand: only the leading block a(1:row, 1:col) is reset
! a = 1.0_DFP
! CALL Reallocate(a, 2_I4B, 3_I4B, isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 16_I4B]), "shape is kept")
! CALL OK(ALL(a(1:2, 1:3) .EQ. 0.0_DFP), "leading block is reset")
! CALL OK(a(3, 1) .EQ. 1.0_DFP, "a(3, 1) is not touched")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, [2_I4B, 3_I4B])
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R2(mat, row, col, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R2

  MODULE PURE SUBROUTINE Reallocate_Real64_R2b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R2b

  MODULE PURE SUBROUTINE Reallocate_Real32_R2(mat, row, col, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R2

  MODULE PURE SUBROUTINE Reallocate_Real32_R2b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-3 array.
!
! Description:
!   Makes `mat` ready to hold an array of shape (i1, i2, i3) and sets
!   the leading block `mat(1:i1, ...)` to zero. The old values
!   are not kept.
!
!   - If `mat` is not allocated, it is allocated with shape (i1, i2, i3).
!     `expandFactor` is not used in this case.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when its shape differs, so that
!     `SHAPE(mat) = [i1, i2, i3]`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, every
!     dimension that is too small grows to `n_k * expandFactor`, and
!     the other dimensions keep their size. If any dimension grows,
!     `mat` is reallocated.
!   - Only the leading block is set. Other entries keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The shape can also be given as a vector `s`; then the first 3
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(3) - array to reallocate
!   i1, i2, i3 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 3`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat, k) >= n_k` for every k, with the leading
!     block set to zero
!
! Formula:
!   $$s_k = \begin{cases}
!     n_k, & \text{mat is not allocated or isExpand is false} \\
!     s_k^{old}, & \text{isExpand}, s_k^{old} \ge n_k \\
!     n_k f, & \text{isExpand}, s_k^{old} < n_k
!   \end{cases}$$
!
!   where $s_k = \mathrm{SIZE(mat, k)}$, $n_k$ is the requested size of
!   dimension $k$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 3_I4B, 4_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape is (2, 3, 4)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! isExpand: grow the last dimension
! CALL Reallocate(a, 2_I4B, 3_I4B, 8_I4B, isExpand=.TRUE., &
!                 expandFactor=1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 8_I4B]), "shape is (2, 3, 8)")
!
! ! isExpand: a large enough array is kept
! CALL Reallocate(a, 2_I4B, 3_I4B, 4_I4B, isExpand=.TRUE., &
!                 expandFactor=2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 8_I4B]), "shape is kept")
!
! ! expandFactor: only the dimensions that are too small grow
! CALL Reallocate(a, 4_I4B, 6_I4B, 4_I4B, isExpand=.TRUE., &
!                 expandFactor=2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [8_I4B, 12_I4B, 8_I4B]), &
!         "shape is (8, 12, 8)")
!
! ! the shape can also be given as a vector
! a = 1.0_DFP
! CALL Reallocate(a, [1_I4B, 2_I4B, 3_I4B])
! CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 2_I4B, 3_I4B]), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R3(mat, i1, i2, i3, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R3

  MODULE PURE SUBROUTINE Reallocate_Real64_R3b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R3b

  MODULE PURE SUBROUTINE Reallocate_Real32_R3(mat, i1, i2, i3, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R3

  MODULE PURE SUBROUTINE Reallocate_Real32_R3b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-4 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 4
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(4) - array to reallocate
!   i1 ... i4 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 4`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 4, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
! INTEGER(I4B) :: s1(4), s2(4)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1.0_DFP
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R4(mat, i1, i2, i3, i4, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R4

  MODULE PURE SUBROUTINE Reallocate_Real64_R4b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R4b

  MODULE PURE SUBROUTINE Reallocate_Real32_R4(mat, i1, i2, i3, i4, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R4

  MODULE PURE SUBROUTINE Reallocate_Real32_R4b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-5 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 5
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(5) - array to reallocate
!   i1 ... i5 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 5`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 5, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :, :)
! INTEGER(I4B) :: s1(5), s2(5)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1.0_DFP
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R5(mat, i1, i2, i3, i4, i5, &
                                              isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R5

  MODULE PURE SUBROUTINE Reallocate_Real64_R5b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R5b

  MODULE PURE SUBROUTINE Reallocate_Real32_R5(mat, i1, i2, i3, i4, i5, &
                                              isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R5

  MODULE PURE SUBROUTINE Reallocate_Real32_R5b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-6 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5, i6)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 6
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(6) - array to reallocate
!   i1 ... i6 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 6`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5, i6]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 6, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :, :, :)
! INTEGER(I4B) :: s1(6), s2(6)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1.0_DFP
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R6(mat, i1, i2, i3, i4, i5, i6, &
                                              isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R6

  MODULE PURE SUBROUTINE Reallocate_Real64_R6b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R6b

  MODULE PURE SUBROUTINE Reallocate_Real32_R6(mat, i1, i2, i3, i4, i5, i6, &
                                              isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R6

  MODULE PURE SUBROUTINE Reallocate_Real32_R6b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real rank-7 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5, i6, i7)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 7
!   entries of `s` are used.
!
! Arguments:
!   mat : REAL (REAL32 or REAL64), ALLOCATABLE,
!     rank(7) - array to reallocate
!   i1 ... i7 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 7`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5, i6, i7]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 7, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :, :, :, :)
! INTEGER(I4B) :: s1(7), s2(7)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1.0_DFP
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R7(mat, i1, i2, i3, i4, i5, &
    & i6, i7, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R7

  MODULE PURE SUBROUTINE Reallocate_Real64_R7b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R7b

  MODULE PURE SUBROUTINE Reallocate_Real32_R7(mat, i1, i2, i3, i4, i5, i6, &
                                              i7, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R7

  MODULE PURE SUBROUTINE Reallocate_Real32_R7b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer vector.
!
! Description:
!   Makes `mat` ready to hold `row` entries and sets `mat(1:row)` to
!   zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated with size
!     `row * expandFactor`. `expandFactor` is used here even when
!     `isExpand` is absent.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when `SIZE(mat) /= row`, so that
!     `SIZE(mat) = row`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, it is kept when
!     `SIZE(mat) >= row`, and reallocated with size
!     `row * expandFactor` otherwise.
!   - Only `mat(1:row)` is set. Entries after `row` keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The size can also be given as a vector `s`; then `row = s(1)`
!   and the other entries of `s` are not used.
!
! Arguments:
!   mat : INTEGER (INT8, INT16, INT32 or INT64), ALLOCATABLE,
!     rank(1) - vector to reallocate
!   row : INTEGER(I4B) - required size, `row >= 0`
!   s : INTEGER(I4B), rank(1) - required size as `s(1)`, `SIZE(s) >= 1`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat) >= row`, with `mat(1:row) = 0`
!
! Formula:
!   $$n = \begin{cases}
!     \mathrm{row} \cdot f, & \text{mat is not allocated} \\
!     \mathrm{row}, & \text{isExpand is false} \\
!     n_{old}, & \text{isExpand}, n_{old} \ge \mathrm{row} \\
!     \mathrm{row} \cdot f, & \text{isExpand}, n_{old} < \mathrm{row}
!   \end{cases}$$
!
!   $$\mathrm{mat}_i = 0, \; i = 1, \dots, \mathrm{row}$$
!
!   where $n = \mathrm{SIZE(mat)}$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 3_I4B)
! CALL OK(SIZE(a) .EQ. 3_I4B, "size is 3")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! a different size without isExpand
! a = 7_I4B
! CALL Reallocate(a, 2_I4B)
! CALL OK(SIZE(a) .EQ. 2_I4B, "size is 2")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! ! not allocated with isExpand and expandFactor
! DEALLOCATE (a)
! CALL Reallocate(a, 3_I4B, isExpand=.TRUE., expandFactor=2_I4B)
! CALL OK(SIZE(a) .EQ. 6_I4B, "size is 3 * 2")
! CALL OK(ALL(a(1:3) .EQ. 0_I4B), "a(1:3) is zero")
!
! ! the size can also be given as a vector
! CALL Reallocate(a, [5_I4B])
! CALL OK(SIZE(a) .EQ. 5_I4B, "size from s(1)")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R1

  MODULE PURE SUBROUTINE Reallocate_Int64_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R1b

  MODULE PURE SUBROUTINE Reallocate_Int32_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1

  MODULE PURE SUBROUTINE Reallocate_Int32_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1b

  MODULE PURE SUBROUTINE Reallocate_Int16_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R1

  MODULE PURE SUBROUTINE Reallocate_Int16_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R1b

  MODULE PURE SUBROUTINE Reallocate_Int8_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R1

  MODULE PURE SUBROUTINE Reallocate_Int8_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-2 array.
!
! Description:
!   Makes `mat` ready to hold an array of shape (row, col) and sets
!   the leading block `mat(1:row, ...)` to zero. The old values
!   are not kept.
!
!   - If `mat` is not allocated, it is allocated with shape (row, col).
!     `expandFactor` is not used in this case.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when its shape differs, so that
!     `SHAPE(mat) = [row, col]`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, every
!     dimension that is too small grows to `n_k * expandFactor`, and
!     the other dimensions keep their size. If any dimension grows,
!     `mat` is reallocated.
!   - Only the leading block is set. Other entries keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The shape can also be given as a vector `s`; then the first 2
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT8, INT16, INT32 or INT64), ALLOCATABLE,
!     rank(2) - array to reallocate
!   row, col : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 2`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat, k) >= n_k` for every k, with the leading
!     block set to zero
!
! Formula:
!   $$s_k = \begin{cases}
!     n_k, & \text{mat is not allocated or isExpand is false} \\
!     s_k^{old}, & \text{isExpand}, s_k^{old} \ge n_k \\
!     n_k f, & \text{isExpand}, s_k^{old} < n_k
!   \end{cases}$$
!
!   where $s_k = \mathrm{SIZE(mat, k)}$, $n_k$ is the requested size of
!   dimension $k$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 3_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape is (2, 3)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! a different shape without isExpand
! a = 5_I4B
! CALL Reallocate(a, 3_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [3_I4B, 2_I4B]), "shape is (3, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! ! isExpand with expandFactor
! CALL Reallocate(a, 4_I4B, 2_I4B, isExpand=.TRUE., expandFactor=2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [8_I4B, 2_I4B]), "shape is (8, 2)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, [1_I4B, 4_I4B])
! CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 4_I4B]), "shape from s")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R2

  MODULE PURE SUBROUTINE Reallocate_Int64_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R2b

  MODULE PURE SUBROUTINE Reallocate_Int32_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R2

  MODULE PURE SUBROUTINE Reallocate_Int32_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R2b

  MODULE PURE SUBROUTINE Reallocate_Int16_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R2

  MODULE PURE SUBROUTINE Reallocate_Int16_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R2b

  MODULE PURE SUBROUTINE Reallocate_Int8_R2(mat, row, col, isExpand, &
                                            expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R2

  MODULE PURE SUBROUTINE Reallocate_Int8_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-3 array.
!
! Description:
!   Makes `mat` ready to hold an array of shape (i1, i2, i3) and sets
!   the leading block `mat(1:i1, ...)` to zero. The old values
!   are not kept.
!
!   - If `mat` is not allocated, it is allocated with shape (i1, i2, i3).
!     `expandFactor` is not used in this case.
!   - If `mat` is allocated and `isExpand` is `.FALSE.` (default), it
!     is reallocated only when its shape differs, so that
!     `SHAPE(mat) = [i1, i2, i3]`.
!   - If `mat` is allocated and `isExpand` is `.TRUE.`, every
!     dimension that is too small grows to `n_k * expandFactor`, and
!     the other dimensions keep their size. If any dimension grows,
!     `mat` is reallocated.
!   - Only the leading block is set. Other entries keep their old
!     value when `mat` is kept, and are undefined when it is newly
!     allocated.
!
!   The shape can also be given as a vector `s`; then the first 3
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT32 or INT64), ALLOCATABLE,
!     rank(3) - array to reallocate
!   i1, i2, i3 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 3`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SIZE(mat, k) >= n_k` for every k, with the leading
!     block set to zero
!
! Formula:
!   $$s_k = \begin{cases}
!     n_k, & \text{mat is not allocated or isExpand is false} \\
!     s_k^{old}, & \text{isExpand}, s_k^{old} \ge n_k \\
!     n_k f, & \text{isExpand}, s_k^{old} < n_k
!   \end{cases}$$
!
!   where $s_k = \mathrm{SIZE(mat, k)}$, $n_k$ is the requested size of
!   dimension $k$ and $f$ is `expandFactor`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :)
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 2_I4B, 2_I4B]), "shape is (2, 2, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! a different shape without isExpand
! a = 3_I4B
! CALL Reallocate(a, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 2_I4B, 3_I4B]), "shape is (1, 2, 3)")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! ! isExpand with expandFactor
! CALL Reallocate(a, 2_I4B, 2_I4B, 3_I4B, isExpand=.TRUE., &
!                 expandFactor=3_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [6_I4B, 2_I4B, 3_I4B]), "shape is (6, 2, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, [2_I4B, 1_I4B, 1_I4B])
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 1_I4B, 1_I4B]), "shape from s")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R3(mat, i1, i2, i3, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R3

  MODULE PURE SUBROUTINE Reallocate_Int64_R3b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R3b

  MODULE PURE SUBROUTINE Reallocate_Int32_R3(mat, i1, i2, i3, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R3

  MODULE PURE SUBROUTINE Reallocate_Int32_R3b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-4 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 4
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT32 or INT64), ALLOCATABLE,
!     rank(4) - array to reallocate
!   i1 ... i4 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 4`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 4, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :)
! INTEGER(I4B) :: s1(4), s2(4)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1_I4B
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0_I4B), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R4(mat, i1, i2, i3, i4, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R4

  MODULE PURE SUBROUTINE Reallocate_Int64_R4b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R4b

  MODULE PURE SUBROUTINE Reallocate_Int32_R4(mat, i1, i2, i3, i4, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R4

  MODULE PURE SUBROUTINE Reallocate_Int32_R4b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-5 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 5
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT32 or INT64), ALLOCATABLE,
!     rank(5) - array to reallocate
!   i1 ... i5 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 5`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 5, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :, :)
! INTEGER(I4B) :: s1(5), s2(5)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1_I4B
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0_I4B), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R5(mat, i1, i2, i3, i4, i5, &
                                             isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R5

  MODULE PURE SUBROUTINE Reallocate_Int64_R5b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R5b

  MODULE PURE SUBROUTINE Reallocate_Int32_R5(mat, i1, i2, i3, i4, i5, &
                                             isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R5

  MODULE PURE SUBROUTINE Reallocate_Int32_R5b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-6 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5, i6)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 6
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT32 or INT64), ALLOCATABLE,
!     rank(6) - array to reallocate
!   i1 ... i6 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 6`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5, i6]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 6, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :, :, :)
! INTEGER(I4B) :: s1(6), s2(6)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1_I4B
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0_I4B), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R6(mat, i1, i2, i3, i4, i5, i6, &
                                             isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R6

  MODULE PURE SUBROUTINE Reallocate_Int64_R6b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R6b

  MODULE PURE SUBROUTINE Reallocate_Int32_R6(mat, i1, i2, i3, i4, i5, i6, &
                                             isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R6

  MODULE PURE SUBROUTINE Reallocate_Int32_R6b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates an integer rank-7 array.
!
! Description:
!   Makes `mat` an array of shape (i1, i2, i3, i4, i5, i6, i7)
!   and sets all entries to zero. The old values are not kept.
!
!   - If `mat` is not allocated, it is allocated.
!   - If `mat` is allocated, it is reallocated only when its shape
!     differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used for this rank.
!
!   The shape can also be given as a vector `s`; then the first 7
!   entries of `s` are used.
!
! Arguments:
!   mat : INTEGER (INT32 or INT64), ALLOCATABLE,
!     rank(7) - array to reallocate
!   i1 ... i7 : INTEGER(I4B) - required size of each dimension
!   s : INTEGER(I4B), rank(1) - required shape, `SIZE(s) >= 7`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   mat : `SHAPE(mat) = [i1, i2, i3, i4, i5, i6, i7]`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE(mat, k)} = i_k, \; k = 1, \dots, 7, \; \mathrm{mat} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :, :, :, :)
! INTEGER(I4B) :: s1(7), s2(7)
!
! s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
! s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]
!
! ! not allocated: allocate and set to zero
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2, 2, 2)")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! same shape: the old values are not kept
! a = 1_I4B
! CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. 0_I4B), "old values are reset")
!
! ! isExpand is ignored for this rank, so the array shrinks
! CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
!                 isExpand=.TRUE.)
! CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 1, 1, 3)")
!
! ! the shape can also be given as a vector
! CALL Reallocate(a, s1)
! CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
! CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R7(mat, i1, i2, i3, i4, i5, &
                                             i6, i7, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R7

  MODULE PURE SUBROUTINE Reallocate_Int64_R7b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R7b

  MODULE PURE SUBROUTINE Reallocate_Int32_R7(mat, i1, i2, i3, i4, i5, i6, &
                                             i7, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R7

  MODULE PURE SUBROUTINE Reallocate_Int32_R7b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates up to six integer vectors.
!
! Description:
!   Reallocates `vec1` and `vec2`, and also `vec3` to `vec6` when they
!   are present, in one call. Each `vec_k` is set to zero and has
!   size `n_k` on return. The old values are not kept.
!
!   - A vector is allocated when it is not allocated, and
!     reallocated only when `SIZE(vec_k) /= n_k`.
!   - When `vec_k` is present, `n_k` must be present too. This is
!     not checked.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used.
!
! Arguments:
!   vec1, vec2 : INTEGER(I4B), ALLOCATABLE, rank(1) - vectors
!     to reallocate
!   n1, n2 : INTEGER(I4B) - required sizes of `vec1` and `vec2`
!   vec3 ... vec6 : INTEGER(I4B), ALLOCATABLE, rank(1),
!     OPTIONAL - more vectors to reallocate
!   n3 ... n6 : INTEGER(I4B), OPTIONAL - required sizes of `vec3` to
!     `vec6`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   vec_k : `SIZE(vec_k) = n_k`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE}(\mathrm{vec}_k) = n_k, \quad \mathrm{vec}_k = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: v1(:), v2(:), v3(:), v4(:), v5(:), v6(:)
!
! ! two vectors
! CALL Reallocate(v1, 3_I4B, v2, 4_I4B)
! CALL OK(SIZE(v1) .EQ. 3_I4B, "size of v1 is 3")
! CALL OK(SIZE(v2) .EQ. 4_I4B, "size of v2 is 4")
! CALL OK(ALL(v1 .EQ. 0_I4B) .AND. ALL(v2 .EQ. 0_I4B), "entries are zero")
! CALL OK(.NOT. ALLOCATED(v3), "v3 is not touched")
!
! ! the old values are not kept
! v1 = 7_I4B
! v2 = 7_I4B
! CALL Reallocate(v1, 3_I4B, v2, 2_I4B)
! CALL OK(ALL(v1 .EQ. 0_I4B), "v1 is reset")
! CALL OK(SIZE(v2) .EQ. 2_I4B, "size of v2 is 2")
!
! ! six vectors
! CALL Reallocate(v1, 1_I4B, v2, 2_I4B, v3, 3_I4B, v4, 4_I4B, &
!                 v5, 5_I4B, v6, 6_I4B)
! CALL OK(SIZE(v1) .EQ. 1_I4B, "size of v1 is 1")
! CALL OK(SIZE(v3) .EQ. 3_I4B, "size of v3 is 3")
! CALL OK(SIZE(v6) .EQ. 6_I4B, "size of v6 is 6")
! CALL OK(ALL(v6 .EQ. 0_I4B), "v6 is zero")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1_6(vec1, n1, vec2, n2, vec3, &
                                               n3, vec4, n4, vec5, n5, vec6, &
                                               n6, isExpand, expandFactor)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    INTEGER(I4B), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates up to six real vectors.
!
! Description:
!   Reallocates `vec1` and `vec2`, and also `vec3` to `vec6` when they
!   are present, in one call. Each `vec_k` is set to zero and has
!   size `n_k` on return. The old values are not kept.
!
!   - A vector is allocated when it is not allocated, and
!     reallocated only when `SIZE(vec_k) /= n_k`.
!   - When `vec_k` is present, `n_k` must be present too. This is
!     not checked.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used.
!
! Arguments:
!   vec1, vec2 : REAL (REAL32 or REAL64), ALLOCATABLE, rank(1) - vectors
!     to reallocate
!   n1, n2 : INTEGER(I4B) - required sizes of `vec1` and `vec2`
!   vec3 ... vec6 : REAL (REAL32 or REAL64), ALLOCATABLE, rank(1),
!     OPTIONAL - more vectors to reallocate
!   n3 ... n6 : INTEGER(I4B), OPTIONAL - required sizes of `vec3` to
!     `vec6`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   vec_k : `SIZE(vec_k) = n_k`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE}(\mathrm{vec}_k) = n_k, \quad \mathrm{vec}_k = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: v1(:), v2(:), v3(:)
!
! ! two vectors
! CALL Reallocate(v1, 2_I4B, v2, 5_I4B)
! CALL OK(SIZE(v1) .EQ. 2_I4B, "size of v1 is 2")
! CALL OK(SIZE(v2) .EQ. 5_I4B, "size of v2 is 5")
! CALL OK(ALL(v2 .EQ. 0.0_DFP), "v2 is zero")
!
! ! three vectors, the old values are not kept
! v1 = 1.0_DFP
! CALL Reallocate(v1, 2_I4B, v2, 1_I4B, v3, 4_I4B)
! CALL OK(ALL(v1 .EQ. 0.0_DFP), "v1 is reset")
! CALL OK(SIZE(v2) .EQ. 1_I4B, "size of v2 is 1")
! CALL OK(SIZE(v3) .EQ. 4_I4B, "size of v3 is 4")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1_6(vec1, n1, vec2, &
                                                n2, vec3, n3, vec4, n4, &
                                                vec5, n5, vec6, n6, &
                                                isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    REAL(REAL64), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1_6

  MODULE PURE SUBROUTINE Reallocate_Real32_R1_6(vec1, n1, vec2, &
                                                n2, vec3, n3, vec4, &
                                                n4, vec5, n5, vec6, &
                                                n6, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    REAL(REAL32), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates the A, IA, JA vectors of a sparse matrix.
!
! Description:
!   Reallocates the real vector `A` and the integer vectors `IA` and
!   `JA` in one call, for example the value, row-pointer and column
!   index vectors of a CSR matrix. On return `SIZE(A) = nA`,
!   `SIZE(IA) = nIA` and `SIZE(JA) = nJA`, and all entries are zero.
!   The old values are not kept.
!
!   - A vector is allocated when it is not allocated, and
!     reallocated only when its size differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used.
!
! Arguments:
!   A : REAL (REAL32 or REAL64), ALLOCATABLE, rank(1) - values
!   nA : INTEGER(I4B) - required size of `A`
!   IA : INTEGER(I4B), ALLOCATABLE, rank(1) - first index vector
!   nIA : INTEGER(I4B) - required size of `IA`
!   JA : INTEGER(I4B), ALLOCATABLE, rank(1) - second index vector
!   nJA : INTEGER(I4B) - required size of `JA`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   A, IA, JA : sizes `nA`, `nIA`, `nJA`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE}(A) = n_A, \; \mathrm{SIZE}(IA) = n_{IA}, \;
!     \mathrm{SIZE}(JA) = n_{JA}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: A(:)
! INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)
!
! ! CSR storage of a 3x3 matrix with 5 nonzeros
! CALL Reallocate(A, 5_I4B, IA, 4_I4B, JA, 5_I4B)
! CALL OK(SIZE(A) .EQ. 5_I4B, "size of A is 5")
! CALL OK(SIZE(IA) .EQ. 4_I4B, "size of IA is 4")
! CALL OK(SIZE(JA) .EQ. 5_I4B, "size of JA is 5")
! CALL OK(ALL(A .EQ. 0.0_DFP), "A is zero")
! CALL OK(ALL(IA .EQ. 0_I4B) .AND. ALL(JA .EQ. 0_I4B), "IA, JA are zero")
!
! ! new sizes, the old values are not kept
! A = 1.0_DFP
! IA = 1_I4B
! CALL Reallocate(A, 7_I4B, IA, 4_I4B, JA, 7_I4B)
! CALL OK(SIZE(A) .EQ. 7_I4B, "size of A is 7")
! CALL OK(ALL(IA .EQ. 0_I4B), "IA is reset")
! CALL OK(SIZE(JA) .EQ. 7_I4B, "size of JA is 7")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AIJ(A, nA, IA, nIA, JA, nJA, &
                                               isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_AIJ

  MODULE PURE SUBROUTINE Reallocate_Real32_AIJ(A, nA, IA, nIA, JA, nJA, &
                                               isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_AIJ
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Allocates or reallocates a real vector and an integer vector.
!
! Description:
!   Reallocates the real vector `A` and the integer vector `IA` in one
!   call. On return `SIZE(A) = nA` and `SIZE(IA) = nIA`, and all
!   entries are zero. The old values are not kept.
!
!   - A vector is allocated when it is not allocated, and
!     reallocated only when its size differs.
!   - `isExpand` and `expandFactor` are accepted for a uniform
!     interface, but they are not used.
!
! Arguments:
!   A : REAL (REAL32 or REAL64), ALLOCATABLE, rank(1) - values
!   nA : INTEGER(I4B) - required size of `A`
!   IA : INTEGER(I4B), ALLOCATABLE, rank(1) - index vector
!   nIA : INTEGER(I4B) - required size of `IA`
!   isExpand : LOGICAL(LGT), OPTIONAL - default `.FALSE.`
!   expandFactor : INTEGER(I4B), OPTIONAL - default 1
!
! Returns:
!   A, IA : sizes `nA` and `nIA`, all entries zero
!
! Formula:
!   $$\mathrm{SIZE}(A) = n_A, \; \mathrm{SIZE}(IA) = n_{IA}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReallocateUtility, ONLY: Reallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: A(:)
! INTEGER(I4B), ALLOCATABLE :: IA(:)
!
! CALL Reallocate(A, 3_I4B, IA, 4_I4B)
! CALL OK(SIZE(A) .EQ. 3_I4B, "size of A is 3")
! CALL OK(SIZE(IA) .EQ. 4_I4B, "size of IA is 4")
! CALL OK(ALL(A .EQ. 0.0_DFP), "A is zero")
! CALL OK(ALL(IA .EQ. 0_I4B), "IA is zero")
!
! ! new sizes, the old values are not kept
! A = 2.0_DFP
! CALL Reallocate(A, 3_I4B, IA, 2_I4B)
! CALL OK(ALL(A .EQ. 0.0_DFP), "A is reset")
! CALL OK(SIZE(IA) .EQ. 2_I4B, "size of IA is 2")
!
! END PROGRAM main
!```

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AI(A, nA, IA, nIA, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_AI

  MODULE PURE SUBROUTINE Reallocate_Real32_AI(A, nA, IA, nIA, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_AI
END INTERFACE Reallocate

END MODULE ReallocateUtility
