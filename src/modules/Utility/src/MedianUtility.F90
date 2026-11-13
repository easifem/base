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
! date: 2023-06-27
! summary: Median of three entries of a vector.
!
! Description:
!   This module contains median-of-three helpers. They are used to
!   choose the pivot in the quick sort part of `IntroSort` and
!   `ArgIntroSort`.
!
!   - `Median` sorts the three entries `this(left)`, `this(mid)` and
!     `this(right)` in place, so that the median ends up in
!     `this(mid)`.
!   - `ArgMedian` does the same on an index vector `indx`, and does not
!     change `this`.
!
!   Both are available for integer (`INT8`, `INT16`, `INT32`, `INT64`)
!   and real (`REAL32`, `REAL64`) vectors.

MODULE MedianUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE

PRIVATE
PUBLIC :: Median
PUBLIC :: ArgMedian

!----------------------------------------------------------------------------
!                                                                  Median
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Sorts three entries of a vector and puts the median in the
! middle.
!
! Description:
!   Sorts the three entries `this(left)`, `this(mid)` and `this(right)`
!   in ascending order, in place. After the call, `this(mid)` is the
!   median of the three values, `this(left)` the smallest and
!   `this(right)` the largest.
!
!   - It uses three compare-and-swap steps: (left, right),
!     (left, mid), then (mid, right).
!   - Only these three entries are changed. All other entries of
!     `this` keep their values.
!   - The order is defined by the arguments, not by the positions.
!     Normally `left < mid < right`, but this is not required: the
!     largest value always goes to the position passed as `right`,
!     even when `right < left`.
!   - Equal values are not swapped, because the comparison is strict.
!   - The indices are not checked. They should be distinct and inside
!     `[1, SIZE(this)]`.
!
!   It is available for `INT8`, `INT16`, `INT32`, `INT64`, `REAL32`
!   and `REAL64`.
!
! Arguments:
!   this : INTEGER or REAL, rank(1), INTENT(INOUT) - vector; three of
!     its entries are sorted in place
!   left : INTEGER(I4B) - position that gets the smallest value
!   mid : INTEGER(I4B) - position that gets the median value
!   right : INTEGER(I4B) - position that gets the largest value
!
! Returns:
!   this : the entries `this(left)`, `this(mid)`, `this(right)` are
!     sorted; other entries are not changed
!
! Formula:
!   $$\mathrm{this}(\mathrm{left}) \le \mathrm{this}(\mathrm{mid})
!     \le \mathrm{this}(\mathrm{right})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MedianUtility, ONLY: Median
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: perms(3, 6), a(3), b(5), ii
! LOGICAL(LGT) :: isok
!
! ! the median ends up in the middle position
! a = [3_I4B, 1_I4B, 2_I4B]
! CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B]), "median of [3, 1, 2]")
! CALL OK(a(2) .EQ. 2_I4B, "median is in this(mid)")
!
! ! all six orderings of three distinct values are sorted
! perms = RESHAPE([1, 2, 3, 1, 3, 2, 2, 1, 3, 2, 3, 1, 3, 1, 2, 3, 2, 1], &
!                 [3, 6])
! isok = .TRUE.
! DO ii = 1, 6
!   a = perms(:, ii)
!   CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
!   isok = isok .AND. ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B])
! END DO
! CALL OK(isok, "all six orderings")
!
! ! equal values
! a = [2_I4B, 1_I4B, 2_I4B]
! CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 2_I4B]), "two equal values")
!
! a = [5_I4B, 5_I4B, 5_I4B]
! CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(a .EQ. 5_I4B), "three equal values")
!
! ! non-adjacent positions, other entries are not changed
! b = [30_I4B, 99_I4B, 10_I4B, 98_I4B, 20_I4B]
! CALL Median(b, 1_I4B, 3_I4B, 5_I4B)
! CALL OK(ALL(b .EQ. [10_I4B, 99_I4B, 20_I4B, 98_I4B, 30_I4B]), &
!         "non-adjacent positions")
!
! ! the order follows the arguments, not the positions
! b = [30_I4B, 99_I4B, 10_I4B, 98_I4B, 20_I4B]
! CALL Median(b, 5_I4B, 3_I4B, 1_I4B)
! CALL OK(ALL(b .EQ. [30_I4B, 99_I4B, 20_I4B, 98_I4B, 10_I4B]), &
!         "left > mid > right")
!
! END PROGRAM main
!```

INTERFACE Median
  MODULE PURE SUBROUTINE Median_Int8(this, left, mid, right)
    INTEGER(INT8), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int8

  MODULE PURE SUBROUTINE Median_Int16(this, left, mid, right)
    INTEGER(INT16), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int16

  MODULE PURE SUBROUTINE Median_Int32(this, left, mid, right)
    INTEGER(INT32), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int32

  MODULE PURE SUBROUTINE Median_Int64(this, left, mid, right)
    INTEGER(INT64), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int64

  MODULE PURE SUBROUTINE Median_Real32(this, left, mid, right)
    REAL(REAL32), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Real32

  MODULE PURE SUBROUTINE Median_Real64(this, left, mid, right)
    REAL(REAL64), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Real64
END INTERFACE Median

!----------------------------------------------------------------------------
!                                                             ArgMedian
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Sorts three entries of an index vector by the values they
! point to.
!
! Description:
!   This is the index version of `Median`. It permutes the three
!   entries `indx(left)`, `indx(mid)` and `indx(right)` so that the
!   values they point to are in ascending order. After the call,
!   `this(indx(mid))` is the median of the three values.
!
!   - `this` is not changed. Only `indx(left)`, `indx(mid)` and
!     `indx(right)` are swapped; other entries of `indx` keep their
!     values.
!   - It uses the same three compare-and-swap steps as `Median`.
!   - Equal values are not swapped, because the comparison is strict.
!   - The entries of `indx` must be valid positions in `this`, and
!     `left`, `mid`, `right` must be valid positions in `indx`. This is
!     not checked.
!
!   It is available for `INT8`, `INT16`, `INT32`, `INT64`, `REAL32`
!   and `REAL64` values; `indx` is always `INTEGER(I4B)`.
!
! Arguments:
!   this : INTEGER or REAL, rank(1), INTENT(IN) - values
!   indx : INTEGER(I4B), rank(1), INTENT(INOUT) - positions in `this`;
!     three of its entries are permuted
!   left : INTEGER(I4B) - position in `indx` that gets the smallest
!     value
!   mid : INTEGER(I4B) - position in `indx` that gets the median value
!   right : INTEGER(I4B) - position in `indx` that gets the largest
!     value
!
! Returns:
!   indx : the entries `indx(left)`, `indx(mid)`, `indx(right)` are
!     permuted; other entries are not changed
!
! Formula:
!   $$\mathrm{this}(\mathrm{indx}(\mathrm{left}))
!     \le \mathrm{this}(\mathrm{indx}(\mathrm{mid}))
!     \le \mathrm{this}(\mathrm{indx}(\mathrm{right}))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MedianUtility, ONLY: ArgMedian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: perms(3, 6), a(3), a0(3), b(5), indx(3), jndx(5), ii
! LOGICAL(LGT) :: isok
!
! ! indx is permuted, the values are not changed
! a = [30_I4B, 10_I4B, 20_I4B]
! a0 = a
! indx = [1_I4B, 2_I4B, 3_I4B]
! CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(indx .EQ. [2_I4B, 3_I4B, 1_I4B]), "indx of [30, 10, 20]")
! CALL OK(a(indx(2)) .EQ. 20_I4B, "median is this(indx(mid))")
! CALL OK(ALL(a .EQ. a0), "this is not changed")
!
! ! all six orderings of three distinct values
! perms = RESHAPE([1, 2, 3, 1, 3, 2, 2, 1, 3, 2, 3, 1, 3, 1, 2, 3, 2, 1], &
!                 [3, 6])
! isok = .TRUE.
! DO ii = 1, 6
!   a = perms(:, ii)
!   indx = [1_I4B, 2_I4B, 3_I4B]
!   CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
!   isok = isok .AND. ALL(a(indx) .EQ. [1_I4B, 2_I4B, 3_I4B])
!   isok = isok .AND. ALL(a .EQ. perms(:, ii))
! END DO
! CALL OK(isok, "all six orderings")
!
! ! equal values
! a = [2_I4B, 1_I4B, 2_I4B]
! indx = [1_I4B, 2_I4B, 3_I4B]
! CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
! CALL OK(ALL(indx .EQ. [2_I4B, 1_I4B, 3_I4B]), "two equal values")
!
! ! non-adjacent positions, other entries of indx are not changed
! b = [50_I4B, 40_I4B, 30_I4B, 20_I4B, 10_I4B]
! jndx = [3_I4B, 1_I4B, 4_I4B, 2_I4B, 5_I4B]
! CALL ArgMedian(b, jndx, 1_I4B, 3_I4B, 5_I4B)
! CALL OK(ALL(jndx .EQ. [5_I4B, 1_I4B, 4_I4B, 2_I4B, 3_I4B]), &
!         "non-adjacent positions")
!
! END PROGRAM main
!```

INTERFACE ArgMedian
  MODULE PURE SUBROUTINE ArgMedian_Int8(this, indx, left, mid, right)
    INTEGER(INT8), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int8

  MODULE PURE SUBROUTINE ArgMedian_Int16(this, indx, left, mid, right)
    INTEGER(INT16), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int16

  MODULE PURE SUBROUTINE ArgMedian_Int32(this, indx, left, mid, right)
    INTEGER(INT32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int32

  MODULE PURE SUBROUTINE ArgMedian_Int64(this, indx, left, mid, right)
    INTEGER(INT64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int64

  MODULE PURE SUBROUTINE ArgMedian_Real32(this, indx, left, mid, right)
    REAL(REAL32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Real32

  MODULE PURE SUBROUTINE ArgMedian_Real64(this, indx, left, mid, right)
    REAL(REAL64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Real64
END INTERFACE ArgMedian

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MedianUtility
