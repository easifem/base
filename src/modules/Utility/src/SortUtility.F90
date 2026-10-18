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
! summary: Routines for sorting vectors in ascending order.
!
! Description:
!   This module contains in-place and indirect (argument) sorting
!   routines for vectors. All routines sort in ascending order.
!
!   In-place sorting, `array` is overwritten by the sorted values:
!
!   - `IntroSort(array)`
!   - `HeapSort(array)`
!   - `InsertionSort(array, low, high)`
!   - `QuickSort(vect1, [vect2, [vect3, [vect4,]]] low, high)`
!
!   Indirect sorting, `array` is not changed and `array(arg)` is sorted:
!
!   - `ArgIntroSort(array, arg)`, `arg` must contain `1..n` on entry
!   - `ArgHeapSort(array, arg)`, `arg` is set by the routine
!   - `ArgInsertionSort(array, arg, low, high)`, `arg` is reordered
!
!   Functions that return a sorted copy or the sort indices, with the
!   name of the algorithm as an optional argument:
!
!   - `Sort(x, name)`
!   - `ArgSort(x, name)`
!
!   The single-vector routines support `INT8`, `INT16`, `INT32`,
!   `INT64`, `REAL32` and `REAL64`. The multi-vector `QuickSort`
!   routines support `INTEGER(I4B)` and `REAL(DFP)` vectors.
!
!   `IntroSort` and `ArgIntroSort` use `Median`/`ArgMedian`
!   (MedianUtility) to choose the pivot and `Partition`/`ArgPartition`
!   (PartitionUtility) to split the vector.
!
!   None of the routines is stable, except `InsertionSort` and
!   `ArgInsertionSort`.
!
! Reference:
!
! https://github.com/leonfoks/coretran/blob/master/src/sorting/m_sort.f90

MODULE SortUtility
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
IMPLICIT NONE
PRIVATE

PUBLIC :: ArgHeapSort
PUBLIC :: HeapSort
PUBLIC :: QuickSort
PUBLIC :: Sort
PUBLIC :: ArgSort
PUBLIC :: InsertionSort
PUBLIC :: ArgInsertionSort
PUBLIC :: IntroSort
PUBLIC :: ArgIntroSort

!----------------------------------------------------------------------------
!                                                           IntroSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Sorts a vector in place by the introsort algorithm.
!
! Description:
!   Sorts `array` in ascending order in place. Introsort is a quick
!   sort that switches to other algorithms:
!
!   - A part with fewer than 16 entries is sorted by `InsertionSort`.
!   - Otherwise the median of `array(left)`, `array(mid)` and
!     `array(right)` is moved to `array(left)` and used as the pivot
!     of `Partition`. Both parts are then sorted recursively.
!   - When the recursion depth reaches
!     `maxDepth = 2 * NINT(LOG(REAL(n)))`, the remaining part is sorted
!     by `HeapSort`. This keeps the cost at $O(n \log n)$ for any
!     input.
!
!   The sort is not stable. It is available for `INT8`, `INT16`,
!   `INT32`, `INT64`, `REAL32` and `REAL64`.
!
!   If `SIZE(array) <= 1` the routine returns at once.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(INOUT) - vector to sort
!
! Returns:
!   array is overwritten with its entries in ascending order
!
! Formula:
!   $$\mathrm{array}_i \le \mathrm{array}_{i+1},
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: IntroSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(10), b(40), c(1), d(8), e(0)
! INTEGER(I4B) :: ii
!
! ! short vector (insertion sort is used)
! a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! CALL IntroSort(a)
! CALL OK(ALL(a .EQ. [(ii, ii=1, 10)]), "short vector")
!
! ! long vector, a permutation of 1..40 (partitioning is used)
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! CALL IntroSort(b)
! CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")
!
! ! already sorted and reverse sorted
! b = [(ii, ii=1, 40)]
! CALL IntroSort(b)
! CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "already sorted")
! b = [(41_I4B - ii, ii=1, 40)]
! CALL IntroSort(b)
! CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "reverse sorted")
!
! ! duplicate entries
! d = [3_I4B, 1_I4B, 3_I4B, 2_I4B, 1_I4B, 3_I4B, 2_I4B, 1_I4B]
! CALL IntroSort(d)
! CALL OK(ALL(d .EQ. [1_I4B, 1_I4B, 1_I4B, 2_I4B, 2_I4B, 3_I4B, &
!                     3_I4B, 3_I4B]), "duplicates")
!
! ! vector of size 1
! c = [42_I4B]
! CALL IntroSort(c)
! CALL OK(c(1) .EQ. 42_I4B, "size 1")
!
! ! empty vector
! CALL IntroSort(e)
! CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")
!
! END PROGRAM main
!```

INTERFACE IntroSort
  MODULE PURE SUBROUTINE IntroSort_Int8(array)
    INTEGER(INT8), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Int8
  MODULE PURE SUBROUTINE IntroSort_Int16(array)
    INTEGER(INT16), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Int16
  MODULE PURE SUBROUTINE IntroSort_Int32(array)
    INTEGER(INT32), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Int32
  MODULE PURE SUBROUTINE IntroSort_Int64(array)
    INTEGER(INT64), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Int64
  MODULE PURE SUBROUTINE IntroSort_Real32(array)
    REAL(REAL32), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Real32
  MODULE PURE SUBROUTINE IntroSort_Real64(array)
    REAL(REAL64), INTENT(INOUT) :: array(:)
  END SUBROUTINE IntroSort_Real64
END INTERFACE IntroSort

!----------------------------------------------------------------------------
!                                                           ArgIntroSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Indirect sort of a vector by the introsort algorithm.
!
! Description:
!   Reorders `arg` so that `array(arg)` is in ascending order.
!   `array` is not changed.
!
!   - `arg` is not initialised by the routine. On entry it must hold
!     the indices to sort, normally `arg = [1, 2, ..., SIZE(array)]`.
!   - `SIZE(arg)` should be at least `SIZE(array)`; only
!     `arg(1:SIZE(array))` is used.
!   - Parts with fewer than 16 entries are sorted by
!     `ArgInsertionSort`. Larger parts are split by `ArgMedian` and
!     `ArgPartition`. The pivot is the median of `array(arg(left))`,
!     `array(arg(mid))` and `array(arg(right))`, as in `IntroSort`.
!   - When the recursion depth `2 * NINT(LOG(REAL(n)))` is used up,
!     the remaining part `arg(left:right)` is sorted by
!     `ArgHeapSort`.
!   - The sort is not stable.
!
!   If `SIZE(array) <= 1` the routine returns at once and `arg` is
!   not changed.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(IN) - vector to sort
!   arg : INTEGER(I4B), rank(1), INTENT(INOUT) - indices,
!     `[1, ..., n]` on entry
!
! Returns:
!   arg is reordered so that array(arg) is in ascending order
!
! Formula:
!   $$\mathrm{array}(\mathrm{arg}_i) \le \mathrm{array}(\mathrm{arg}_{i+1}),
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: ArgIntroSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(10), a0(10), arg(10)
! INTEGER(I4B) :: b(40), b0(40), argb(40)
! INTEGER(I4B) :: e(0), arge(0), c(1), argc(1)
! INTEGER(I4B) :: ii
!
! ! arg must contain 1..n on entry
! a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! a0 = a
! arg = [(ii, ii=1, 10)]
! CALL ArgIntroSort(a, arg)
! CALL OK(ALL(arg .EQ. [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, &
!                       5_I4B, 7_I4B, 3_I4B, 9_I4B]), "arg")
! CALL OK(ALL(a(arg) .EQ. [(ii, ii=1, 10)]), "a(arg) is sorted")
! CALL OK(ALL(a .EQ. a0), "array is not changed")
!
! ! long vector, a permutation of 1..40
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! b0 = b
! argb = [(ii, ii=1, 40)]
! CALL ArgIntroSort(b, argb)
! CALL OK(ALL(b(argb) .EQ. [(ii, ii=1, 40)]), "long vector")
! CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
!         "arg is a permutation")
! CALL OK(ALL(b .EQ. b0), "long array is not changed")
!
! ! duplicate entries
! b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
! argb = [(ii, ii=1, 40)]
! CALL ArgIntroSort(b, argb)
! CALL OK(ALL(b(argb(1:39)) .LE. b(argb(2:40))), "duplicates")
! CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
!         "arg is a permutation with duplicates")
!
! ! vector of size 1 and empty vector
! c = [42_I4B]
! argc = [1_I4B]
! CALL ArgIntroSort(c, argc)
! CALL OK(argc(1) .EQ. 1_I4B, "size 1")
! CALL ArgIntroSort(e, arge)
! CALL OK(SIZE(arge) .EQ. 0_I4B, "empty vector")
!
! END PROGRAM main
!```

INTERFACE ArgIntroSort
  MODULE PURE SUBROUTINE ArgIntroSort_Int8(array, arg)
    INTEGER(INT8), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Int8

  MODULE PURE SUBROUTINE ArgIntroSort_Int16(array, arg)
    INTEGER(INT16), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Int16

  MODULE PURE SUBROUTINE ArgIntroSort_Int32(array, arg)
    INTEGER(INT32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Int32

  MODULE PURE SUBROUTINE ArgIntroSort_Int64(array, arg)
    INTEGER(INT64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Int64

  MODULE PURE SUBROUTINE ArgIntroSort_Real32(array, arg)
    REAL(REAL32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Real32

  MODULE PURE SUBROUTINE ArgIntroSort_Real64(array, arg)
    REAL(REAL64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
  END SUBROUTINE ArgIntroSort_Real64
END INTERFACE ArgIntroSort

!----------------------------------------------------------------------------
!                                                           InsertionSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Sorts a part of a vector in place by insertion sort.
!
! Description:
!   Sorts `array(low:high)` in ascending order in place. Entries
!   outside `low:high` are not changed. If `low >= high`, nothing is
!   done.
!
!   - The sort is stable: equal values keep their order.
!   - The cost is $O(m^2)$ for $m = \mathrm{high} - \mathrm{low} + 1$,
!     so it is meant for short vectors. `IntroSort` uses it for parts
!     with fewer than 16 entries.
!   - `low` and `high` are not checked against the bounds of `array`.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(INOUT) - vector
!   low : INTEGER(I4B) - first index of the part to sort
!   high : INTEGER(I4B) - last index of the part to sort
!
! Returns:
!   array(low:high) is overwritten in ascending order
!
! Formula:
!   $$\mathrm{array}_i \le \mathrm{array}_{i+1},
!     \quad i = \mathrm{low}, \dots, \mathrm{high}-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: InsertionSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(8), e(0)
!
! ! the whole vector
! a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! CALL InsertionSort(a, 1_I4B, 8_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 6_I4B, 7_I4B, &
!                     8_I4B]), "whole vector")
!
! ! only a(3:6) is sorted, the other entries are not changed
! a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! CALL InsertionSort(a, 3_I4B, 6_I4B)
! CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 1_I4B, 6_I4B, 7_I4B, 9_I4B, 3_I4B, &
!                     2_I4B]), "sub-range a(3:6)")
!
! ! low = high and low > high do nothing
! a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! CALL InsertionSort(a, 5_I4B, 5_I4B)
! CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, &
!                     2_I4B]), "low = high")
! CALL InsertionSort(a, 6_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, &
!                     2_I4B]), "low > high")
!
! ! empty vector
! CALL InsertionSort(e, 1_I4B, 0_I4B)
! CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")
!
! END PROGRAM main
!```

INTERFACE InsertionSort
  MODULE PURE SUBROUTINE InsertionSort_Int8(array, low, high)
    INTEGER(INT8), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Int8
  MODULE PURE SUBROUTINE InsertionSort_Int16(array, low, high)
    INTEGER(INT16), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Int16
  MODULE PURE SUBROUTINE InsertionSort_Int32(array, low, high)
    INTEGER(INT32), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Int32
  MODULE PURE SUBROUTINE InsertionSort_Int64(array, low, high)
    INTEGER(INT64), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Int64
  MODULE PURE SUBROUTINE InsertionSort_Real32(array, low, high)
    REAL(REAL32), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Real32
  MODULE PURE SUBROUTINE InsertionSort_Real64(array, low, high)
    REAL(REAL64), INTENT(INOUT) :: array(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE InsertionSort_Real64
END INTERFACE InsertionSort

!----------------------------------------------------------------------------
!                                                           ArgInsertionSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Indirect insertion sort of a part of a vector.
!
! Description:
!   Reorders `arg(low:high)` so that `array(arg(low:high))` is in
!   ascending order. `array` and the other entries of `arg` are not
!   changed.
!
!   - `arg` is not initialised by the routine. On entry it must hold
!     valid indices of `array`, normally `arg = [1, ..., SIZE(array)]`.
!   - The sort is stable: indices of equal values keep their order.
!   - If `low >= high`, nothing is done. `low` and `high` are not
!     checked against the bounds of `arg`.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(IN) - vector to sort
!   arg : INTEGER(I4B), rank(1), INTENT(INOUT) - indices into `array`
!   low : INTEGER(I4B) - first index of `arg` to sort
!   high : INTEGER(I4B) - last index of `arg` to sort
!
! Returns:
!   arg(low:high) is reordered so that array(arg(low:high)) is sorted
!
! Formula:
!   $$\mathrm{array}(\mathrm{arg}_i) \le \mathrm{array}(\mathrm{arg}_{i+1}),
!     \quad i = \mathrm{low}, \dots, \mathrm{high}-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: ArgInsertionSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(8), a0(8), arg(8)
! INTEGER(I4B) :: ii
!
! a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! a0 = a
!
! ! arg must contain 1..n on entry
! arg = [(ii, ii=1, 8)]
! CALL ArgInsertionSort(a, arg, 1_I4B, 8_I4B)
! ! the sort is stable: the two 1s keep their order (3 before 5)
! CALL OK(ALL(arg .EQ. [3_I4B, 5_I4B, 8_I4B, 7_I4B, 1_I4B, 4_I4B, &
!                       6_I4B, 2_I4B]), "arg")
! CALL OK(ALL(a(arg(1:7)) .LE. a(arg(2:8))), "a(arg) is sorted")
! CALL OK(ALL(a .EQ. a0), "array is not changed")
!
! ! only arg(5:8) is sorted, arg(1:4) is not changed
! arg = [(ii, ii=1, 8)]
! CALL ArgInsertionSort(a, arg, 5_I4B, 8_I4B)
! CALL OK(ALL(arg .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 8_I4B, &
!                       7_I4B, 6_I4B]), "sub-range arg(5:8)")
!
! END PROGRAM main
!```

INTERFACE ArgInsertionSort
  MODULE PURE SUBROUTINE ArgInsertionSort_Int8(array, arg, low, high)
    INTEGER(INT8), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Int8

  MODULE PURE SUBROUTINE ArgInsertionSort_Int16(array, arg, low, high)
    INTEGER(INT16), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Int16

  MODULE PURE SUBROUTINE ArgInsertionSort_Int32(array, arg, low, high)
    INTEGER(INT32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Int32

  MODULE PURE SUBROUTINE ArgInsertionSort_Int64(array, arg, low, high)
    INTEGER(INT64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Int64

  MODULE PURE SUBROUTINE ArgInsertionSort_Real32(array, arg, low, high)
    REAL(REAL32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Real32

  MODULE PURE SUBROUTINE ArgInsertionSort_Real64(array, arg, low, high)
    REAL(REAL64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(INOUT) :: arg(:)
    INTEGER(I4B), INTENT(IN) :: low
    INTEGER(I4B), INTENT(IN) :: high
  END SUBROUTINE ArgInsertionSort_Real64
END INTERFACE ArgInsertionSort

!----------------------------------------------------------------------------
!                                                              HeapSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Sorts a vector in place by heap sort.
!
! Description:
!   Sorts `array` in ascending order in place by heap sort. The cost
!   is $O(n \log n)$ for any input and no extra memory is used.
!
!   - The sort is not stable.
!   - Vectors with `SIZE(array) <= 1` are returned unchanged.
!   - It is available for `INT8`, `INT16`, `INT32`, `INT64`, `REAL32`
!     and `REAL64`.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(INOUT) - vector to sort
!
! Returns:
!   array is overwritten with its entries in ascending order
!
! Formula:
!   $$\mathrm{array}_i \le \mathrm{array}_{i+1},
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: HeapSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(10), b(40), c(1), e(0)
! INTEGER(I4B) :: ii
!
! a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! CALL HeapSort(a)
! CALL OK(ALL(a .EQ. [(ii, ii=1, 10)]), "short vector")
!
! ! a permutation of 1..40
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! CALL HeapSort(b)
! CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")
!
! ! duplicates
! b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
! CALL HeapSort(b)
! CALL OK(ALL(b(1:39) .LE. b(2:40)), "duplicates")
! CALL OK(COUNT(b .EQ. 4_I4B) .EQ. 8_I4B, "duplicates are kept")
!
! ! size 1 and size 0
! c = [42_I4B]
! CALL HeapSort(c)
! CALL OK(c(1) .EQ. 42_I4B, "size 1")
! CALL HeapSort(e)
! CALL OK(SIZE(e) .EQ. 0_I4B, "size 0")
!
! END PROGRAM main
!```

INTERFACE HeapSort
  MODULE PURE SUBROUTINE HeapSort_Int8(array)
    INTEGER(INT8), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int8
  MODULE PURE SUBROUTINE HeapSort_Int16(array)
    INTEGER(INT16), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int16
  MODULE PURE SUBROUTINE HeapSort_Int32(array)
    INTEGER(INT32), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int32
  MODULE PURE SUBROUTINE HeapSort_Int64(array)
    INTEGER(INT64), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int64
  MODULE PURE SUBROUTINE HeapSort_Real32(array)
    REAL(REAL32), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Real32
  MODULE PURE SUBROUTINE HeapSort_Real64(array)
    REAL(REAL64), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Real64
END INTERFACE HeapSort

!----------------------------------------------------------------------------
!                                                           ArgHeapSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Indirect sort of a vector by heap sort.
!
! Description:
!   Returns `arg` such that `array(arg)` is in ascending order.
!   `array` is not changed.
!
!   - `arg` is `INTENT(OUT)`: the routine first sets
!     `arg = [1, 2, ..., SIZE(arg)]`, so its value on entry is not
!     used.
!   - The dummy argument is declared `arg(0:)`, but the values stored
!     in it are the usual 1-based indices of `array`.
!   - Only `array(1:SIZE(arg))` is sorted, so `SIZE(arg)` should be
!     equal to `SIZE(array)`.
!   - The sort is not stable.
!
! Arguments:
!   array : INTEGER or REAL, rank(1), INTENT(IN) - vector to sort
!   arg : INTEGER(I4B), rank(1), INTENT(OUT) - sort indices,
!     `SIZE(arg) = SIZE(array)`
!
! Returns:
!   arg such that array(arg) is in ascending order
!
! Formula:
!   $$\mathrm{array}(\mathrm{arg}_i) \le \mathrm{array}(\mathrm{arg}_{i+1}),
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: ArgHeapSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(10), a0(10), arg(10)
! INTEGER(I4B) :: b(40), argb(40), e(0), arge(0)
! INTEGER(I4B) :: ii
!
! a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! a0 = a
!
! ! arg is set by the routine, its value on entry is not used
! arg = 0_I4B
! CALL ArgHeapSort(a, arg)
! CALL OK(ALL(arg .EQ. [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, &
!                       5_I4B, 7_I4B, 3_I4B, 9_I4B]), "arg")
! CALL OK(ALL(a(arg) .EQ. [(ii, ii=1, 10)]), "a(arg) is sorted")
! CALL OK(ALL(a .EQ. a0), "array is not changed")
!
! ! duplicates
! b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
! CALL ArgHeapSort(b, argb)
! CALL OK(ALL(b(argb(1:39)) .LE. b(argb(2:40))), "duplicates")
! CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
!         "arg is a permutation")
!
! ! empty vector
! CALL ArgHeapSort(e, arge)
! CALL OK(SIZE(arge) .EQ. 0_I4B, "empty vector")
!
! END PROGRAM main
!```

INTERFACE ArgHeapSort
  MODULE PURE SUBROUTINE ArgHeapSort_Int8(array, arg)
    INTEGER(INT8), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int8

  MODULE PURE SUBROUTINE ArgHeapSort_Int16(array, arg)
    INTEGER(INT16), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int16

  MODULE PURE SUBROUTINE ArgHeapSort_Int32(array, arg)
    INTEGER(INT32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int32

  MODULE PURE SUBROUTINE ArgHeapSort_Int64(array, arg)
    INTEGER(INT64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int64

  MODULE PURE SUBROUTINE ArgHeapSort_Real32(array, arg)
    REAL(REAL32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Real32

  MODULE PURE SUBROUTINE ArgHeapSort_Real64(array, arg)
    REAL(REAL64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Real64
END INTERFACE ArgHeapSort

!----------------------------------------------------------------------------
!                                                             QuickSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Sorts a part of a vector in place.
!
! Description:
!   Sorts `vect1(low:high)` in ascending order in place. Entries
!   outside `low:high` are not changed. If `low >= high`, nothing is
!   done, so an empty vector can be passed with `low = 1, high = 0`.
!
!   - It calls `IntroSort` on `vect1(low:high)`, so the cost is
!     $O(n \log n)$ and the recursion depth is $O(\log n)$ for every
!     input, including sorted, reverse sorted and constant vectors.
!   - The sort is not stable.
!   - It is available for `INT8`, `INT16`, `INT32`, `INT64`, `REAL32`
!     and `REAL64`.
!
! Arguments:
!   vect1 : INTEGER or REAL, rank(1), INTENT(INOUT) - vector
!   low : INTEGER(I4B) - first index of the part to sort
!   high : INTEGER(I4B) - last index of the part to sort
!
! Returns:
!   vect1(low:high) is overwritten in ascending order
!
! Formula:
!   $$\mathrm{vect1}_i \le \mathrm{vect1}_{i+1},
!     \quad i = \mathrm{low}, \dots, \mathrm{high}-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: QuickSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(8), b(40), e(0)
! INTEGER(I4B) :: ii
!
! ! the whole vector
! a = [4_I4B, 8_I4B, 1_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! CALL QuickSort(a, 1_I4B, 8_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 6_I4B, 7_I4B, &
!                     8_I4B]), "whole vector")
!
! ! only a(3:6) is sorted, the other entries are not changed
! a = [4_I4B, 8_I4B, 9_I4B, 6_I4B, 1_I4B, 7_I4B, 3_I4B, 2_I4B]
! CALL QuickSort(a, 3_I4B, 6_I4B)
! CALL OK(ALL(a .EQ. [4_I4B, 8_I4B, 1_I4B, 6_I4B, 7_I4B, 9_I4B, 3_I4B, &
!                     2_I4B]), "sub-range a(3:6)")
!
! ! a permutation of 1..40
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! CALL QuickSort(b, 1_I4B, 40_I4B)
! CALL OK(ALL(b .EQ. [(ii, ii=1, 40)]), "long vector")
!
! ! empty vector (high < low)
! CALL QuickSort(e, 1_I4B, 0_I4B)
! CALL OK(SIZE(e) .EQ. 0_I4B, "empty vector")
!
! END PROGRAM main
!```

INTERFACE QuickSort
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt8(vect1, low, high)
    INTEGER(INT8), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt8
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt16(vect1, low, high)
    INTEGER(INT16), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt16
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt32(vect1, low, high)
    INTEGER(INT32), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt32
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt64(vect1, low, high)
    INTEGER(INT64), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt64
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectReal32(vect1, low, high)
    REAL(REAL32), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectReal32
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectReal64(vect1, low, high)
    REAL(REAL64), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectReal64
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                             QuickSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Sorts two vectors by the entries of the first one.
!
! Description:
!   Sorts `vect1(low:high)` in ascending order and
!   applies the same permutation to `vect2(low:high)`, so that the
!   pairs `(vect1(i), vect2(i))` stay together.
!
!   - Only `vect1` is compared. Ties in `vect1` are not broken by
!     `vect2`, and their order is not defined (the sort is not stable).
!   - `vect1` and `vect2` can each be `INTEGER(I4B)` or `REAL(DFP)`,
!     which gives four combinations.
!   - The order is found by `ArgIntroSort` on `vect1(low:high)` and
!     then applied to every vector, so the cost is $O(n \log n)$ for
!     every input. A temporary index vector of size
!     `high - low + 1` is used.
!   - `SIZE(vect2)` should be at least `high`. This is not checked.
!
! Arguments:
!   vect1 : INTEGER(I4B) or REAL(DFP), rank(1), INTENT(INOUT) - key
!   vect2 : INTEGER(I4B) or REAL(DFP), rank(1), INTENT(INOUT) - follows
!     `vect1`
!   low : INTEGER(I4B) - first index of the part to sort
!   high : INTEGER(I4B) - last index of the part to sort
!
! Returns:
!   vect1(low:high) is in ascending order, vect2 is permuted with it
!
! Formula:
!   $$\mathrm{vect1}_i \le \mathrm{vect1}_{i+1}, \quad
!     (\mathrm{vect1}_i, \mathrm{vect2}_i) =
!     (\mathrm{vect1}^0_{p(i)}, \mathrm{vect2}^0_{p(i)})$$
!
!   where $p$ is the permutation applied by the sort and the
!   superscript $0$ denotes the input.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: QuickSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: v1(6)
! INTEGER(I4B) :: v2(6)
! INTEGER(I4B) :: ii
!
! ! vect1 is the key, the other vectors follow it
! v1 = [4_I4B, 2_I4B, 5_I4B, &
!       2_I4B, 1_I4B, 3_I4B]
! v2 = [(ii, ii=1, 6)]
!
! CALL QuickSort(v1, v2, 1_I4B, 6_I4B)
!
! ! vect1 is sorted in ascending order
! CALL OK(ALL(v1 .EQ. [1_I4B, 2_I4B, 2_I4B, &
!                      3_I4B, 4_I4B, 5_I4B]), "vect1 is sorted")
!
! ! v2 holds the original row number of each entry of v1
! CALL OK(ALL(v1 .EQ. KEY0(v2)), "rows stay together")
! CALL OK(ALL([(COUNT(v2 .EQ. ii) .EQ. 1, ii=1, 6)]), &
!         "vect2 is a permutation")
!
! CONTAINS
!
! PURE FUNCTION KEY0(i) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: i(:)
!   INTEGER(I4B) :: ans(SIZE(i))
!   INTEGER(I4B) :: k(6)
!   k = [4_I4B, 2_I4B, 5_I4B, &
!        2_I4B, 1_I4B, 3_I4B]
!   ans = k(i)
! END FUNCTION KEY0
!
! END PROGRAM main
!```

INTERFACE QuickSort
  MODULE RECURSIVE PURE SUBROUTINE QuickSort2vectIR(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort2vectIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectII(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort2vectII

  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectRI(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort2vectRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectRR(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort2vectRR
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                             QuickSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Sorts three vectors by the entries of the first one.
!
! Description:
!   Sorts `vect1(low:high)` in ascending order and
!   applies the same permutation to `vect2(low:high)` and
!   `vect3(low:high)`, so that the rows
!   `(vect1(i), vect2(i), vect3(i))` stay together.
!
!   - Only `vect1` is compared. Ties in `vect1` are not broken by the
!     other vectors, and their order is not defined.
!   - Each vector can be `INTEGER(I4B)` or `REAL(DFP)`, which gives
!     eight combinations (III, IIR, IRR, IRI, RRR, RRI, RIR, RII).
!   - The order is found by `ArgIntroSort` on `vect1(low:high)` and
!     then applied to every vector, so the cost is $O(n \log n)$ for
!     every input. A temporary index vector of size
!     `high - low + 1` is used.
!
! Arguments:
!   vect1 : INTEGER(I4B) or REAL(DFP), rank(1), INTENT(INOUT) - key
!   vect2, vect3 : INTEGER(I4B) or REAL(DFP), rank(1), INTENT(INOUT)
!     - follow `vect1`
!   low : INTEGER(I4B) - first index of the part to sort
!   high : INTEGER(I4B) - last index of the part to sort
!
! Returns:
!   vect1(low:high) is in ascending order, vect2 and vect3 are
!   permuted with it
!
! Formula:
!   $$\mathrm{vect1}_i \le \mathrm{vect1}_{i+1}, \quad
!     \mathrm{vect}k_i = \mathrm{vect}k^0_{p(i)}, \; k = 1, 2, 3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: QuickSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: v1(6)
! INTEGER(I4B) :: v2(6)
! INTEGER(I4B) :: v3(6)
! INTEGER(I4B) :: ii
!
! ! vect1 is the key, the other vectors follow it
! v1 = [4_I4B, 2_I4B, 5_I4B, &
!       2_I4B, 1_I4B, 3_I4B]
! v2 = [(ii, ii=1, 6)]
! v3 = [(10_I4B * ii, ii=1, 6)]
!
! CALL QuickSort(v1, v2, v3, 1_I4B, 6_I4B)
!
! ! vect1 is sorted in ascending order
! CALL OK(ALL(v1 .EQ. [1_I4B, 2_I4B, 2_I4B, &
!                      3_I4B, 4_I4B, 5_I4B]), "vect1 is sorted")
!
! ! v2 holds the original row number of each entry of v1
! CALL OK(ALL(v1 .EQ. KEY0(v2)), "rows stay together")
! CALL OK(ALL([(COUNT(v2 .EQ. ii) .EQ. 1, ii=1, 6)]), &
!         "vect2 is a permutation")
! CALL OK(ALL(v3 .EQ. 10_I4B * v2), "vect3 follows vect1")
!
! CONTAINS
!
! PURE FUNCTION KEY0(i) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: i(:)
!   INTEGER(I4B) :: ans(SIZE(i))
!   INTEGER(I4B) :: k(6)
!   k = [4_I4B, 2_I4B, 5_I4B, &
!        2_I4B, 1_I4B, 3_I4B]
!   ans = k(i)
! END FUNCTION KEY0
!
! END PROGRAM main
!```

INTERFACE QuickSort
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIII(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectIII

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIIR(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectIIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIRR(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectIRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIRI(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectIRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRRR(vect1, vect2, vect3, &
                                                     low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectRRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRRI(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectRRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRIR(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectRIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRII(vect1, vect2, vect3, &
                                                     low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort3vectRII
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                             QuickSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Sorts four vectors by the entries of the first one.
!
! Description:
!   Sorts `vect1(low:high)` in ascending order and
!   applies the same permutation to `vect2`, `vect3` and `vect4`
!   (entries `low:high`), so that the rows
!   `(vect1(i), vect2(i), vect3(i), vect4(i))` stay together.
!
!   - Only `vect1` is compared. Ties in `vect1` are not broken by the
!     other vectors, and their order is not defined.
!   - Each vector can be `INTEGER(I4B)` or `REAL(DFP)`, which gives
!     all sixteen combinations (IIII, IIIR, ..., RIII).
!   - The order is found by `ArgIntroSort` on `vect1(low:high)` and
!     then applied to every vector, so the cost is $O(n \log n)$ for
!     every input. A temporary index vector of size
!     `high - low + 1` is used.
!
! Arguments:
!   vect1 : INTEGER(I4B) or REAL(DFP), rank(1), INTENT(INOUT) - key
!   vect2, vect3, vect4 : INTEGER(I4B) or REAL(DFP), rank(1),
!     INTENT(INOUT) - follow `vect1`
!   low : INTEGER(I4B) - first index of the part to sort
!   high : INTEGER(I4B) - last index of the part to sort
!
! Returns:
!   vect1(low:high) is in ascending order, the other vectors are
!   permuted with it
!
! Formula:
!   $$\mathrm{vect1}_i \le \mathrm{vect1}_{i+1}, \quad
!     \mathrm{vect}k_i = \mathrm{vect}k^0_{p(i)}, \; k = 1, \dots, 4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: QuickSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: v1(6)
! INTEGER(I4B) :: v2(6)
! INTEGER(I4B) :: v3(6)
! INTEGER(I4B) :: v4(6)
! INTEGER(I4B) :: ii
!
! ! vect1 is the key, the other vectors follow it
! v1 = [4_I4B, 2_I4B, 5_I4B, &
!       2_I4B, 1_I4B, 3_I4B]
! v2 = [(ii, ii=1, 6)]
! v3 = [(10_I4B * ii, ii=1, 6)]
! v4 = [(100_I4B * ii, ii=1, 6)]
!
! CALL QuickSort(v1, v2, v3, v4, 1_I4B, 6_I4B)
!
! ! vect1 is sorted in ascending order
! CALL OK(ALL(v1 .EQ. [1_I4B, 2_I4B, 2_I4B, &
!                      3_I4B, 4_I4B, 5_I4B]), "vect1 is sorted")
!
! ! v2 holds the original row number of each entry of v1
! CALL OK(ALL(v1 .EQ. KEY0(v2)), "rows stay together")
! CALL OK(ALL([(COUNT(v2 .EQ. ii) .EQ. 1, ii=1, 6)]), &
!         "vect2 is a permutation")
! CALL OK(ALL(v3 .EQ. 10_I4B * v2), "vect3 follows vect1")
! CALL OK(ALL(v4 .EQ. 100_I4B * v2), "vect4 follows vect1")
!
! CONTAINS
!
! PURE FUNCTION KEY0(i) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: i(:)
!   INTEGER(I4B) :: ans(SIZE(i))
!   INTEGER(I4B) :: k(6)
!   k = [4_I4B, 2_I4B, 5_I4B, &
!        2_I4B, 1_I4B, 3_I4B]
!   ans = k(i)
! END FUNCTION KEY0
!
! END PROGRAM main
!```

INTERFACE QuickSort
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIII(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIIII

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIIR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIIIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIRI(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIIRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIRR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIIRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRRR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIRRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRRI(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIRRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRIR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIRIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRII(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectIRII

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRRR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRRRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRRI(vect1, vect2, &
                                                      vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRRRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRIR(vect1, vect2, &
                                                      vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRRIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRII(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRRII

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIRR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRIRR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIRI(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRIRI

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIIR(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRIIR

  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIII(vect1, vect2, vect3, &
                                                      vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort4vectRIII
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                                 Sort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a sorted copy of a vector.
!
! Description:
!   Returns the entries of `x` in ascending order. `x` is not changed.
!   The optional `name` selects the algorithm. It is converted to
!   upper case, so it is not case sensitive:
!
!   - `"INTROSORT"` (default): `IntroSort`
!   - `"QUICKSORT"`: `QuickSort`
!   - `"HEAPSORT"`: `HeapSort`
!   - `"INSERTIONSORT"`: `InsertionSort`
!
!   Any other name (for example `"INSERTION"`, which `ArgSort`
!   accepts) is silently ignored and `x` is returned unsorted.
!
! Arguments:
!   x : INTEGER or REAL, rank(1) - vector to sort
!   name : CHARACTER(*), OPTIONAL - name of the algorithm
!
! Returns:
!   ans : same type and kind as `x`, `SIZE(ans) = SIZE(x)` - the
!     sorted entries of `x`
!
! Formula:
!   $$\mathrm{ans}_i \le \mathrm{ans}_{i+1},
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: Sort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(10), y(10), b(40)
! INTEGER(I4B) :: ii
!
! x = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! y = [(ii, ii=1, 10)]
!
! ! the default algorithm is INTROSORT
! CALL OK(ALL(Sort(x) .EQ. y), "default")
! CALL OK(ALL(Sort(x, "INTROSORT") .EQ. y), "INTROSORT")
! CALL OK(ALL(Sort(x, "QUICKSORT") .EQ. y), "QUICKSORT")
! CALL OK(ALL(Sort(x, "HEAPSORT") .EQ. y), "HEAPSORT")
! CALL OK(ALL(Sort(x, "INSERTIONSORT") .EQ. y), "INSERTIONSORT")
!
! ! the name is not case sensitive
! CALL OK(ALL(Sort(x, "HeapSort") .EQ. y), "HeapSort")
!
! ! an unknown name returns x unchanged
! CALL OK(ALL(Sort(x, "BUBBLESORT") .EQ. x), "unknown name")
! CALL OK(ALL(Sort(x, "INSERTION") .EQ. x), "INSERTION is unknown")
!
! ! x itself is not changed
! CALL OK(x(1) .EQ. 5_I4B, "x is not changed")
!
! ! a permutation of 1..40
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! CALL OK(ALL(Sort(b) .EQ. [(ii, ii=1, 40)]), "long vector")
!
! END PROGRAM main
!```

INTERFACE Sort
  MODULE PURE FUNCTION Sort_Int8(x, name) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(INT8) :: ans(SIZE(x))
  END FUNCTION Sort_Int8
  MODULE PURE FUNCTION Sort_Int16(x, name) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(INT16) :: ans(SIZE(x))
  END FUNCTION Sort_Int16
  MODULE PURE FUNCTION Sort_Int32(x, name) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(INT32) :: ans(SIZE(x))
  END FUNCTION Sort_Int32
  MODULE PURE FUNCTION Sort_Int64(x, name) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(INT64) :: ans(SIZE(x))
  END FUNCTION Sort_Int64
  MODULE PURE FUNCTION Sort_Real32(x, name) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    REAL(REAL32) :: ans(SIZE(x))
  END FUNCTION Sort_Real32
  MODULE PURE FUNCTION Sort_Real64(x, name) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    REAL(REAL64) :: ans(SIZE(x))
  END FUNCTION Sort_Real64
END INTERFACE Sort

!----------------------------------------------------------------------------
!                                                                    ArgSort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns the indices that sort a vector.
!
! Description:
!   Returns `ans` such that `x(ans)` is in ascending order. `x` is not
!   changed. The optional `name` selects the algorithm. It is
!   converted to upper case, so it is not case sensitive:
!
!   - `"INTROSORT"` (default): `ArgIntroSort`
!   - `"QUICKSORT"`: also `ArgIntroSort`
!   - `"HEAPSORT"`: `ArgHeapSort`
!   - `"INSERTION"`: `ArgInsertionSort`
!
!   Any other name (for example `"INSERTIONSORT"`, which `Sort`
!   accepts) is silently ignored and `ans = [1, 2, ..., SIZE(x)]` is
!   returned.
!
!   The default and `"QUICKSORT"` use `ArgIntroSort`.
!
! Arguments:
!   x : INTEGER or REAL, rank(1) - vector to sort
!   name : CHARACTER(*), OPTIONAL - name of the algorithm
!
! Returns:
!   ans : INTEGER(I4B), `SIZE(ans) = SIZE(x)` - indices such that
!     `x(ans)` is sorted
!
! Formula:
!   $$x(\mathrm{ans}_i) \le x(\mathrm{ans}_{i+1}),
!     \quad i = 1, \dots, n-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SortUtility, ONLY: ArgSort
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(10), arg(10), id(10), b(40), argb(40)
! INTEGER(I4B) :: ii
!
! x = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
!      10_I4B, 4_I4B]
! arg = [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, 5_I4B, 7_I4B, &
!        3_I4B, 9_I4B]
! id = [(ii, ii=1, 10)]
!
! ! the default algorithm is INTROSORT
! CALL OK(ALL(ArgSort(x) .EQ. arg), "default")
! CALL OK(ALL(x(ArgSort(x)) .EQ. id), "x(ArgSort(x)) is sorted")
! CALL OK(ALL(ArgSort(x, "INTROSORT") .EQ. arg), "INTROSORT")
! CALL OK(ALL(ArgSort(x, "QUICKSORT") .EQ. arg), "QUICKSORT")
! CALL OK(ALL(ArgSort(x, "HEAPSORT") .EQ. arg), "HEAPSORT")
! CALL OK(ALL(ArgSort(x, "INSERTION") .EQ. arg), "INSERTION")
!
! ! the name is not case sensitive
! CALL OK(ALL(ArgSort(x, "heapsort") .EQ. arg), "heapsort")
!
! ! an unknown name returns 1..n
! CALL OK(ALL(ArgSort(x, "BUBBLESORT") .EQ. id), "unknown name")
! CALL OK(ALL(ArgSort(x, "INSERTIONSORT") .EQ. id), &
!         "INSERTIONSORT is unknown")
!
! ! a permutation of 1..40
! b = [(MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
! argb = ArgSort(b)
! CALL OK(ALL(b(argb) .EQ. [(ii, ii=1, 40)]), "long vector")
!
! END PROGRAM main
!```

INTERFACE ArgSort
  MODULE PURE FUNCTION ArgSort_Int8(x, name) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int8
  MODULE PURE FUNCTION ArgSort_Int16(x, name) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int16
  MODULE PURE FUNCTION ArgSort_Int32(x, name) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int32
  MODULE PURE FUNCTION ArgSort_Int64(x, name) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int64
  MODULE PURE FUNCTION ArgSort_Real32(x, name) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Real32
  MODULE PURE FUNCTION ArgSort_Real64(x, name) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Real64
END INTERFACE ArgSort

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SortUtility
