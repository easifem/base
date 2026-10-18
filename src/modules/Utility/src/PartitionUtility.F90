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
! summary: Partition methods for quicksorting and quickselect
!
! Description:
!   This module contains a Hoare style partitioning algorithm used by
!   the quicksort and quickselect routines (for example `IntroSort`
!   and `ArgIntroSort`).
!
!   - `Partition` reorders `this(left:right)` in place.
!   - `ArgPartition` reorders the index vector `idx(left:right)` and
!     leaves the values `this` unchanged.
!
!   In both cases the first entry of the range is the pivot. The
!   routines do not choose the pivot themselves. The sort routines call
!   `Median` first and move the median to `left`.
!
!   Both are available for `INT8`, `INT16`, `INT32`, `INT64`, `REAL32`
!   and `REAL64` values.
!
! Reference:
!
!   https://github.com/leonfoks/coretran/blob/master/src/core/m_partition.f90

MODULE PartitionUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE
PUBLIC :: partition
PUBLIC :: argPartition

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Partitioning used for quickSort and quickSelect routines
!
! Description:
!   Partitions `this(left:right)` in place around the pivot
!   `p = this(left)`, the value at `left` on entry.
!
!   - On return the pivot is at `this(iPivot)`.
!   - Every entry of `this(left:iPivot-1)` is less than or equal to
!     the pivot.
!   - Every entry of `this(iPivot+1:right)` is strictly greater than
!     the pivot. Entries equal to the pivot are put on the left.
!   - Entries outside `left:right` are not changed.
!   - The value of `iPivot` on entry is not used.
!
!   The pivot is not chosen by this routine. To use a median of three
!   pivot, call `Median(this, left, mid, right)` and swap `this(mid)`
!   with `this(left)` before calling `Partition`.
!
!   Note: `1 <= left <= right <= SIZE(this)` is not checked.
!
! Arguments:
!   this : INTEGER or REAL, rank(1), INTENT(INOUT) - vector to be
!     partitioned, only `this(left:right)` is changed
!   left : INTEGER(I4B) - first index of the range, also the pivot
!   right : INTEGER(I4B) - last index of the range
!   iPivot : INTEGER(I4B), INTENT(INOUT) - final index of the pivot
!
! Returns:
!   this : partitioned vector
!   iPivot : index of the pivot, `left <= iPivot <= right`
!
! Formula:
!   $$\mathrm{this}_k \le p \;\; (left \le k < i_p), \quad
!   \mathrm{this}_{i_p} = p, \quad
!   \mathrm{this}_k > p \;\; (i_p < k \le right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE PartitionUtility, ONLY: Partition
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(7), b(6), c(5), d(2), e(4), ip
!
! ! pivot is a(1) = 5
! a = [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]
! ip = 0_I4B
! CALL Partition(a, 1_I4B, 7_I4B, ip)
! CALL OK(ip .EQ. 4_I4B, "pivot index")
! CALL OK(a(ip) .EQ. 5_I4B, "pivot value")
! CALL OK(ALL(a(1:ip - 1) .LE. a(ip)), "left part <= pivot")
! CALL OK(ALL(a(ip + 1:7) .GT. a(ip)), "right part > pivot")
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 5_I4B, 9_I4B, 8_I4B, 7_I4B]), &
!         "partitioned vector")
!
! ! only b(2:5) is partitioned, the pivot is b(2) = 4
! b = [10_I4B, 4_I4B, 6_I4B, 1_I4B, 3_I4B, 20_I4B]
! CALL Partition(b, 2_I4B, 5_I4B, ip)
! CALL OK(ip .EQ. 4_I4B, "pivot index of b(2:5)")
! CALL OK(ALL(b .EQ. [10_I4B, 1_I4B, 3_I4B, 4_I4B, 6_I4B, 20_I4B]), &
!         "partitioned b(2:5)")
! CALL OK(b(1) .EQ. 10_I4B .AND. b(6) .EQ. 20_I4B, "outside is untouched")
!
! ! equal values, the pivot ends at the right end
! c = [3_I4B, 3_I4B, 3_I4B, 3_I4B, 99_I4B]
! CALL Partition(c, 1_I4B, 4_I4B, ip)
! CALL OK(ip .EQ. 4_I4B, "equal values")
! CALL OK(ALL(c(1:4) .EQ. 3_I4B) .AND. c(5) .EQ. 99_I4B, "values kept")
!
! ! a single entry
! d = [7_I4B, 1_I4B]
! CALL Partition(d, 1_I4B, 1_I4B, ip)
! CALL OK(ip .EQ. 1_I4B .AND. ALL(d .EQ. [7_I4B, 1_I4B]), "single entry")
!
! ! the pivot is the smallest value
! e = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
! CALL Partition(e, 1_I4B, 4_I4B, ip)
! CALL OK(ip .EQ. 1_I4B, "smallest pivot")
! CALL OK(ALL(e .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "sorted input kept")
!
! END PROGRAM main
!```

INTERFACE partition
  MODULE PURE SUBROUTINE partition_Real32(this, left, right, iPivot)
    REAL(REAL32), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_Real32

  MODULE PURE SUBROUTINE partition_Real64(this, left, right, iPivot)
    REAL(REAL64), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_Real64

  MODULE PURE SUBROUTINE partition_Int8(this, left, right, iPivot)
    INTEGER(INT8), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_Int8

  MODULE PURE SUBROUTINE partition_Int16(this, left, right, iPivot)
    INTEGER(INT16), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_Int16

  MODULE PURE SUBROUTINE partition_Int32(this, left, right, iPivot)
    INTEGER(INT32), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_Int32

  MODULE PURE SUBROUTINE partition_int64(this, left, right, iPivot)
    INTEGER(INT64), INTENT(INOUT) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(IN) :: left
    !! Left index
    INTEGER(I4B), INTENT(IN) :: right
    !! Right index
    INTEGER(I4B), INTENT(INOUT) :: iPivot
    !! Pivoting index
  END SUBROUTINE partition_int64

END INTERFACE

!----------------------------------------------------------------------------
!                                                             argPartition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Partitioning used for argQuicksort routines
!
! Description:
!   Partitions the index vector `idx(left:right)` in place so that the
!   values `this(idx(:))` are partitioned around the pivot
!   `p = this(idx(left))`. The values `this` are not changed.
!
!   - On return the pivot index is at `idx(i)`.
!   - `this(idx(left:i-1))` are less than or equal to the pivot.
!   - `this(idx(i+1:right))` are strictly greater than the pivot.
!   - `idx(left:right)` is a permutation of its input, and entries of
!     `idx` outside `left:right` are not changed.
!   - The value of `i` on entry is not used.
!
!   As in `Partition`, the pivot is not chosen by this routine, and the
!   ranges are not checked.
!
! Arguments:
!   this : INTEGER or REAL, rank(1), INTENT(IN) - values
!   idx : INTEGER(I4B), rank(1), INTENT(INOUT) - indices into `this`,
!     only `idx(left:right)` is changed
!   left : INTEGER(I4B) - first position in `idx`, also the pivot
!   right : INTEGER(I4B) - last position in `idx`
!   i : INTEGER(I4B), INTENT(INOUT) - final position of the pivot
!
! Returns:
!   idx : permuted indices
!   i : position of the pivot in `idx`, `left <= i <= right`
!
! Formula:
!   $$\mathrm{this}_{idx_k} \le p \;\; (left \le k < i), \quad
!   \mathrm{this}_{idx_i} = p, \quad
!   \mathrm{this}_{idx_k} > p \;\; (i < k \le right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE PartitionUtility, ONLY: ArgPartition
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: t(7), idx(7), t2(6), idx2(6), i, k
!
! ! pivot is t(idx(1)) = 5
! t = [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]
! idx = [(k, k=1, 7)]
! i = 0_I4B
! CALL ArgPartition(t, idx, 1_I4B, 7_I4B, i)
! CALL OK(i .EQ. 4_I4B, "pivot position")
! CALL OK(t(idx(i)) .EQ. 5_I4B, "pivot value")
! CALL OK(ALL(t(idx(1:i - 1)) .LE. t(idx(i))), "left part <= pivot")
! CALL OK(ALL(t(idx(i + 1:7)) .GT. t(idx(i))), "right part > pivot")
! CALL OK(ALL(idx .EQ. [4_I4B, 2_I4B, 6_I4B, 1_I4B, 5_I4B, 3_I4B, 7_I4B]), &
!         "permuted indices")
! CALL OK(ALL([(ANY(idx .EQ. k), k=1, 7)]), "idx is a permutation")
! CALL OK(ALL(t .EQ. [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]), &
!         "values are untouched")
!
! ! only idx2(2:5) is permuted
! t2 = [10_I4B, 4_I4B, 6_I4B, 1_I4B, 3_I4B, 20_I4B]
! idx2 = [(k, k=1, 6)]
! CALL ArgPartition(t2, idx2, 2_I4B, 5_I4B, i)
! CALL OK(i .EQ. 4_I4B, "pivot position in idx2(2:5)")
! CALL OK(ALL(idx2 .EQ. [1_I4B, 4_I4B, 5_I4B, 2_I4B, 3_I4B, 6_I4B]), &
!         "permuted idx2(2:5)")
! CALL OK(idx2(1) .EQ. 1_I4B .AND. idx2(6) .EQ. 6_I4B, &
!         "outside is untouched")
!
! END PROGRAM main
!```

INTERFACE argPartition
  MODULE PURE SUBROUTINE argPartition_Real32(this, idx, left, right, i)
    REAL(REAL32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Real32

  MODULE PURE SUBROUTINE argPartition_Real64(this, idx, left, right, i)
    REAL(REAL64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Real64

  MODULE PURE SUBROUTINE argPartition_Int8(this, idx, left, right, i)
    INTEGER(INT8), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Int8

  MODULE PURE SUBROUTINE argPartition_Int16(this, idx, left, right, i)
    INTEGER(INT16), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Int16

  MODULE PURE SUBROUTINE argPartition_Int32(this, idx, left, right, i)
    INTEGER(INT32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Int32

  MODULE PURE SUBROUTINE argPartition_Int64(this, idx, left, right, i)
    INTEGER(INT64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: idx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: right
    INTEGER(I4B), INTENT(INOUT) :: i
  END SUBROUTINE argPartition_Int64

END INTERFACE argPartition

END MODULE PartitionUtility
