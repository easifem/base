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
! date: 2021-11-11
! summary: Utility routines for integer vectors and multi-indices.
!
! Description:
!   This module contains utility routines for integers.
!
!   - `SIZE`: number of multi-indices of a given order.
!   - `GetMultiIndices`, `GetMultiIndices_`: list of multi-indices of a
!     given order.
!   - `OPERATOR(.in.)`: test if a scalar or a set is inside a set.
!   - `OPERATOR(.isin.)`: entrywise test if entries are inside a set.
!   - `RemoveDuplicates`, `RemoveDuplicates_`: remove repeated entries.
!   - `Repeat`: repeat a vector several times.
!   - `GetIndex`: location of a value inside a vector.
!   - `Get`: entries of a vector by index, indices or a section.
!   - `GetIntersection`: common entries of two vectors.
!   - `Get1DIndexFortran`: 1D index of an entry of a 2D, 3D or 4D array.
!
!   The integer routines are available for `INT8`, `INT16`, `INT32` and
!   `INT64`, unless stated otherwise.

MODULE IntegerUtility
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

PUBLIC :: OPERATOR(.in.)
PUBLIC :: OPERATOR(.isin.)
PUBLIC :: RemoveDuplicates
PUBLIC :: RemoveDuplicates_
PUBLIC :: Repeat
PUBLIC :: SIZE
PUBLIC :: GetMultiIndices
PUBLIC :: GetMultiIndices_
PUBLIC :: GetIndex
PUBLIC :: Get
PUBLIC :: GetIntersection
PUBLIC :: Get1DIndexFortran

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Number of multi-indices of order n in d+1 entries.
!
! Description:
!   Returns the number of tuples of `d+1` non-negative integers whose
!   sum is `n`. This is the number of columns of
!   `GetMultiIndices(n, d)`.
!
!   This generic extends the intrinsic `SIZE`, so `SIZE(array)` still
!   works when this module is used.
!
! Arguments:
!   n : INTEGER(I4B) - order (sum of the entries), `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!
! Returns:
!   ans : INTEGER(I4B) - number of tuples
!
! Formula:
!   $$\mathrm{ans} = \binom{n+d}{d}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: SIZE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(5)
!
! ! d = 1: tuples (i, n-i), i = 0, ..., n
! CALL OK(SIZE(3_I4B, 1_I4B) .EQ. 4_I4B, "Size(3, 1)")
!
! ! d = 2: binomial coefficient (n+2 choose 2)
! CALL OK(SIZE(2_I4B, 2_I4B) .EQ. 6_I4B, "Size(2, 2)")
! CALL OK(SIZE(n=3_I4B, d=2_I4B) .EQ. 10_I4B, "Size(3, 2)")
!
! ! d = 3
! CALL OK(SIZE(2_I4B, 3_I4B) .EQ. 10_I4B, "Size(2, 3)")
!
! ! order zero has only one tuple
! CALL OK(SIZE(0_I4B, 2_I4B) .EQ. 1_I4B, "Size(0, 2)")
!
! ! d = 0: the only tuple of one entry is [n]
! CALL OK(SIZE(3_I4B, 0_I4B) .EQ. 1_I4B, "Size(3, 0)")
! CALL OK(SIZE(3_I4B, 0_I4B, .TRUE.) .EQ. 4_I4B, "Size(3, 0, .TRUE.)")
!
! ! the intrinsic SIZE is still available
! x = 0_I4B
! CALL OK(SIZE(x) .EQ. 5_I4B, "intrinsic SIZE")
!
! END PROGRAM main
!```

INTERFACE Size
  MODULE PURE FUNCTION obj_Size1(n, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size1
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Number of multi-indices of order up to n in d+1 entries.
!
! Description:
!   Returns the number of tuples of `d+1` non-negative integers whose
!   sum is at most `n`. This is the number of columns of
!   `GetMultiIndices(n, d, upto)`.
!
!   If `upto` is `.FALSE.`, the result is `SIZE(n, d)`, the number of
!   tuples whose sum is exactly `n`.
!
! Arguments:
!   n : INTEGER(I4B) - highest order, `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!   upto : LOGICAL(LGT) - `.TRUE.`: orders `0..n`, `.FALSE.`: order `n`
!
! Returns:
!   ans : INTEGER(I4B) - number of tuples
!
! Formula:
!   $$\mathrm{ans} = \sum_{i=0}^{n} \binom{i+d}{d}
!     = \binom{n+d+1}{d+1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: SIZE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ii, total
!
! ! d = 1: 1 + 2 + 3 + 4
! CALL OK(SIZE(3_I4B, 1_I4B, .TRUE.) .EQ. 10_I4B, "Size(3, 1, .TRUE.)")
!
! ! d = 2: 1 + 3 + 6
! CALL OK(SIZE(2_I4B, 2_I4B, upto=.TRUE.) .EQ. 10_I4B, &
!         "Size(2, 2, .TRUE.)")
!
! ! order zero
! CALL OK(SIZE(0_I4B, 2_I4B, .TRUE.) .EQ. 1_I4B, "Size(0, 2, .TRUE.)")
!
! ! the result is the sum of Size(ii, d) for ii = 0, ..., n
! total = 0_I4B
! DO ii = 0, 3
!   total = total + SIZE(ii, 2_I4B)
! END DO
! CALL OK(SIZE(3_I4B, 2_I4B, .TRUE.) .EQ. total, "sum of Size(ii, d)")
! CALL OK(total .EQ. 20_I4B, "Size(3, 2, .TRUE.) is 20")
!
! ! upto = .FALSE. counts only the tuples of order n
! CALL OK(SIZE(3_I4B, 1_I4B, .FALSE.) .EQ. SIZE(3_I4B, 1_I4B), &
!         "Size(3, 1, .FALSE.) = Size(3, 1)")
! CALL OK(SIZE(2_I4B, 2_I4B, upto=.FALSE.) .EQ. 6_I4B, &
!         "Size(2, 2, .FALSE.) is 6")
!
! END PROGRAM main
!```

INTERFACE Size
  MODULE PURE FUNCTION obj_Size2(n, d, upto) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size2
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Multi-indices of order n in d+1 entries.
!
! Description:
!   Returns all tuples of `d+1` non-negative integers whose sum is `n`.
!   Each column of `ans` is one tuple.
!
!   - The columns are in increasing lexicographic order. For `n = 2`
!     and `d = 1` the columns are `(0, 2)`, `(1, 1)`, `(2, 0)`.
!   - `d` should be at least zero; negative `d` is not checked. For
!     `d = 0` the only tuple of one entry is `[n]`.
!
! Arguments:
!   n : INTEGER(I4B) - order (sum of the entries), `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - multi-indices,
!     `SHAPE(ans) = [d+1, SIZE(n, d)]`
!
! Formula:
!   $$\mathrm{ans} = \left\{ \alpha \in \mathbb{N}_0^{d+1} :
!     \sum_{k=1}^{d+1} \alpha_k = n \right\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetMultiIndices
! USE IntegerUtility, ONLY: SIZE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: ans(:, :)
! INTEGER(I4B) :: want1(2, 3), want2(3, 6)
!
! ! d = 1, n = 2
! want1 = RESHAPE([0_I4B, 2_I4B, 1_I4B, 1_I4B, 2_I4B, 0_I4B], &
!                 [2_I4B, 3_I4B])
! ans = GetMultiIndices(2_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(ans) .EQ. [2_I4B, 3_I4B]), "shape for d = 1")
! CALL OK(ALL(ans .EQ. want1), "indices for d = 1")
!
! ! d = 2, n = 2
! want2 = RESHAPE([0_I4B, 0_I4B, 2_I4B, 0_I4B, 1_I4B, 1_I4B, &
!                  0_I4B, 2_I4B, 0_I4B, 1_I4B, 0_I4B, 1_I4B, &
!                  1_I4B, 1_I4B, 0_I4B, 2_I4B, 0_I4B, 0_I4B], &
!                 [3_I4B, 6_I4B])
! ans = GetMultiIndices(n=2_I4B, d=2_I4B)
! CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 6_I4B]), "shape for d = 2")
! CALL OK(ALL(ans .EQ. want2), "indices for d = 2")
!
! ! every column sums to n
! ans = GetMultiIndices(4_I4B, 3_I4B)
! CALL OK(SIZE(ans, 2) .EQ. SIZE(4_I4B, 3_I4B), "number of columns")
! CALL OK(ALL(SUM(ans, 1) .EQ. 4_I4B), "each column sums to n")
!
! ! d = 0: the only tuple of one entry is [n]
! ans = GetMultiIndices(3_I4B, 0_I4B)
! CALL OK(ALL(SHAPE(ans) .EQ. [1_I4B, 1_I4B]), "shape for d = 0")
! CALL OK(ans(1, 1) .EQ. 3_I4B, "index for d = 0")
! ans = GetMultiIndices(2_I4B, 0_I4B, .TRUE.)
! CALL OK(ALL(SHAPE(ans) .EQ. [1_I4B, 3_I4B]), "shape for d = 0, upto")
! CALL OK(ALL(ans(1, :) .EQ. [0_I4B, 1_I4B, 2_I4B]), &
!         "indices for d = 0, upto")
!
! END PROGRAM main
!```

INTERFACE GetMultiIndices
  MODULE RECURSIVE PURE FUNCTION obj_GetMultiIndices1(n, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices1
END INTERFACE GetMultiIndices

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Multi-indices of order n in d+1 entries, without allocation.
!
! Description:
!   Same as `GetMultiIndices(n, d)`, but the result is written into
!   `ans`, which is not allocated by this routine.
!
!   - `ans` should have at least `d+1` rows and `SIZE(n, d)` columns.
!     This is not checked.
!   - Only `ans(1:nrow, 1:ncol)` is written.
!
! Arguments:
!   n : INTEGER(I4B) - order (sum of the entries), `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!   ans : INTEGER(I4B), rank(2) - multi-indices in `ans(1:nrow, 1:ncol)`
!   nrow : INTEGER(I4B) - number of rows written, `nrow = d+1`
!   ncol : INTEGER(I4B) - number of columns written,
!     `ncol = SIZE(n, d)`
!
! Formula:
!   $$\mathrm{ans}(:, 1{:}\mathrm{ncol}) = \left\{
!     \alpha \in \mathbb{N}_0^{d+1} :
!     \sum_{k=1}^{d+1} \alpha_k = n \right\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetMultiIndices
! USE IntegerUtility, ONLY: GetMultiIndices_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ans(3, 6), big(4, 10)
! INTEGER(I4B) :: nrow, ncol
!
! ! ans has the exact size: (d+1) by Size(n, d)
! CALL GetMultiIndices_(2_I4B, 2_I4B, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is d+1")
! CALL OK(ncol .EQ. 6_I4B, "ncol is Size(n, d)")
! CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
!         "same as GetMultiIndices")
!
! ! ans can be larger than needed, only ans(1:nrow, 1:ncol) is used
! big = -1_I4B
! CALL GetMultiIndices_(n=2_I4B, d=2_I4B, ans=big, nrow=nrow, ncol=ncol)
! CALL OK(ALL(big(1:nrow, 1:ncol) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
!         "larger ans")
! CALL OK(ALL(big(4, :) .EQ. -1_I4B), "extra row is not changed")
!
! ! d = 0: the only tuple of one entry is [n]
! big = -1_I4B
! CALL GetMultiIndices_(n=3_I4B, d=0_I4B, ans=big, nrow=nrow, ncol=ncol)
! CALL OK(nrow .EQ. 1_I4B .AND. ncol .EQ. 1_I4B, "d = 0: nrow, ncol")
! CALL OK(big(1, 1) .EQ. 3_I4B, "d = 0: the tuple is [n]")
! CALL OK(big(2, 1) .EQ. -1_I4B, "d = 0: rest is not changed")
!
! END PROGRAM main
!```

INTERFACE GetMultiIndices_
  MODULE RECURSIVE PURE SUBROUTINE obj_GetMultiIndices1_(n, d, ans, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n, d
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMultiIndices1_
END INTERFACE GetMultiIndices_

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Multi-indices of order up to n in d+1 entries.
!
! Description:
!   Returns all tuples of `d+1` non-negative integers whose sum is at
!   most `n`. Each column of `ans` is one tuple.
!
!   - The columns of order 0 come first, then order 1, and so on up
!     to order `n`. Inside each order the columns are the same as in
!     `GetMultiIndices(i, d)`.
!   - If `upto` is `.FALSE.`, the result is `GetMultiIndices(n, d)`,
!     the tuples whose sum is exactly `n`.
!   - `d` should be at least zero; negative `d` is not checked. For
!     `d = 0` the only tuple of one entry is `[n]`.
!
! Arguments:
!   n : INTEGER(I4B) - highest order, `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!   upto : LOGICAL(LGT) - `.TRUE.`: orders `0..n`, `.FALSE.`: order `n`
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - multi-indices,
!     `SHAPE(ans) = [d+1, SIZE(n, d, .TRUE.)]`
!
! Formula:
!   $$\mathrm{ans} = \left\{ \alpha \in \mathbb{N}_0^{d+1} :
!     \sum_{k=1}^{d+1} \alpha_k \le n \right\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetMultiIndices
! USE IntegerUtility, ONLY: SIZE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: ans(:, :)
! INTEGER(I4B) :: want(2, 3)
!
! ! d = 1, n = 1: order 0 first, then order 1
! want = RESHAPE([0_I4B, 0_I4B, 0_I4B, 1_I4B, 1_I4B, 0_I4B], &
!                [2_I4B, 3_I4B])
! ans = GetMultiIndices(1_I4B, 1_I4B, .TRUE.)
! CALL OK(ALL(SHAPE(ans) .EQ. [2_I4B, 3_I4B]), "shape for d = 1")
! CALL OK(ALL(ans .EQ. want), "indices for d = 1")
!
! ! d = 2, n = 2: columns are sorted by order
! ans = GetMultiIndices(2_I4B, 2_I4B, upto=.TRUE.)
! CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 10_I4B]), "shape for d = 2")
! CALL OK(SIZE(ans, 2) .EQ. SIZE(2_I4B, 2_I4B, .TRUE.), &
!         "number of columns")
! CALL OK(ALL(SUM(ans, 1) .EQ. [0_I4B, 1_I4B, 1_I4B, 1_I4B, 2_I4B, &
!                               2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]), &
!         "order of each column")
!
! ! the last columns are the multi-indices of order n
! CALL OK(ALL(ans(:, 5:10) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
!         "last columns have order n")
!
! ! upto = .FALSE. gives only the multi-indices of order n
! ans = GetMultiIndices(2_I4B, 2_I4B, upto=.FALSE.)
! CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 6_I4B]), "upto = .FALSE.: shape")
! CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
!         "upto = .FALSE.: same as GetMultiIndices(n, d)")
!
! END PROGRAM main
!```

INTERFACE GetMultiIndices
  MODULE RECURSIVE PURE FUNCTION obj_GetMultiIndices2(n, d, upto) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices2
END INTERFACE GetMultiIndices

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-04
! summary: Multi-indices of order up to n, without allocation.
!
! Description:
!   Same as `GetMultiIndices(n, d, upto)`, but the result is written
!   into `ans`, which is not allocated by this routine.
!
!   - `ans` should have at least `d+1` rows and `SIZE(n, d, .TRUE.)`
!     columns. This is not checked.
!   - If `upto` is `.FALSE.`, the result is `GetMultiIndices(n, d)`,
!     the tuples whose sum is exactly `n`.
!
! Arguments:
!   n : INTEGER(I4B) - highest order, `n >= 0`
!   d : INTEGER(I4B) - the tuples have `d+1` entries, `d >= 0`
!   upto : LOGICAL(LGT) - `.TRUE.`: orders `0..n`, `.FALSE.`: order `n`
!   ans : INTEGER(I4B), rank(2) - multi-indices in `ans(1:nrow, 1:ncol)`
!   nrow : INTEGER(I4B) - number of rows written, `nrow = d+1`
!   ncol : INTEGER(I4B) - number of columns written,
!     `ncol = SIZE(n, d, .TRUE.)`
!
! Formula:
!   $$\mathrm{ans}(:, 1{:}\mathrm{ncol}) = \left\{
!     \alpha \in \mathbb{N}_0^{d+1} :
!     \sum_{k=1}^{d+1} \alpha_k \le n \right\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetMultiIndices
! USE IntegerUtility, ONLY: GetMultiIndices_
! USE IntegerUtility, ONLY: SIZE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ans(3, 10)
! INTEGER(I4B) :: nrow, ncol
!
! CALL GetMultiIndices_(2_I4B, 2_I4B, .TRUE., ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is d+1")
! CALL OK(ncol .EQ. SIZE(2_I4B, 2_I4B, .TRUE.), &
!         "ncol is Size(n, d, .TRUE.)")
! CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B, .TRUE.)), &
!         "same as GetMultiIndices")
!
! ! the first column is the multi-index of order zero
! CALL OK(ALL(ans(:, 1) .EQ. 0_I4B), "first column is zero")
!
! ! upto = .FALSE. gives only the multi-indices of order n
! ans = -1_I4B
! CALL GetMultiIndices_(2_I4B, 2_I4B, .FALSE., ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, &
!         "upto = .FALSE.: nrow = 3, ncol = 6")
! CALL OK(ALL(ans(:, 1:6) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
!         "upto = .FALSE.: same as GetMultiIndices(n, d)")
! CALL OK(ALL(ans(:, 7:10) .EQ. -1_I4B), "upto = .FALSE.: rest unchanged")
!
! END PROGRAM main
!```

INTERFACE GetMultiIndices_
  MODULE RECURSIVE PURE SUBROUTINE obj_GetMultiIndices2_(n, d, upto, ans, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n, d
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMultiIndices2_
END INTERFACE GetMultiIndices_

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if an integer set is a subset of another set.
!
! Description:
!   Returns `.TRUE.` if every entry of `a` is also an entry of `b`.
!
!   - The order of the entries does not matter.
!   - `a` may have repeated entries, so `SIZE(a)` may be larger than
!     `SIZE(b)`.
!   - Defined binary operators have the lowest precedence, so write
!     `.NOT. (a .in. b)` with parentheses.
!
! Arguments:
!   a : INTEGER, rank(1) - set to test
!   b : INTEGER, rank(1) - reference set, same kind as `a`
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if `a` is a subset of `b`
!
! Formula:
!   $$\mathrm{ans} = \left( a_i \in b \quad \forall i \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: OPERATOR(.in.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(2), b(4)
!
! a = [12_I4B, 9_I4B]
! b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
!
! ! every entry of a is in b
! CALL OK(a .in. b, "a is a subset of b")
!
! ! b has entries which are not in a
! CALL OK(.NOT. (b .in. a), "b is not a subset of a")
!
! ! one entry is missing
! CALL OK(.NOT. ([9_I4B, 13_I4B] .in. b), "one entry is missing")
!
! ! the order does not matter
! CALL OK([12_I4B, 10_I4B, 9_I4B] .in. b, "order does not matter")
!
! ! a set is a subset of itself
! CALL OK(b .in. b, "b is a subset of b")
!
! ! a may have more entries than b if some entries are repeated
! CALL OK([9_I4B, 9_I4B, 12_I4B, 9_I4B, 12_I4B] .in. [9_I4B, 12_I4B], &
!         "repeated entries, SIZE(a) > SIZE(b)")
! CALL OK(.NOT. ([9_I4B, 9_I4B, 13_I4B] .in. [9_I4B, 12_I4B]), &
!         "repeated entries with one missing")
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.in.)
  MODULE PURE FUNCTION in_1a(a, b) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1a

  MODULE PURE FUNCTION in_1b(a, b) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1b

  MODULE PURE FUNCTION in_1c(a, b) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1c

  MODULE PURE FUNCTION in_1d(a, b) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1d
END INTERFACE OPERATOR(.in.)

!----------------------------------------------------------------------------
!                                           Operator(.isin.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Entrywise test if the entries of a vector are inside a set.
!
! Description:
!   Returns a vector of logicals. `ans(i)` is `.TRUE.` if `a(i)` is an
!   entry of `b`, otherwise `.FALSE.`.
!
!   - The result has the same size as `a`.
!   - Use `ALL(a .isin. b)` to test if `a` is a subset of `b`.
!
! Arguments:
!   a : INTEGER, rank(1) - entries to test
!   b : INTEGER, rank(1) - reference set, same kind as `a`
!
! Returns:
!   ans : LOGICAL(LGT), rank(1) - entrywise result,
!     `SIZE(ans) = SIZE(a)`
!
! Formula:
!   $$\mathrm{ans}_i = \left( a_i \in b \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntegerUtility, ONLY: OPERATOR(.isin.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(4), b(2)
! LOGICAL(LGT) :: ans4(4), ans2(2)
!
! a = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
! b = [12_I4B, 9_I4B]
!
! ! ans(i) is true if a(i) is in b
! ans4 = a .isin. b
! CALL OK(ALL(ans4 .EQV. [.TRUE., .FALSE., .FALSE., .TRUE.]), &
!         "a .isin. b")
!
! ans2 = b .isin. a
! CALL OK(ALL(ans2), "b .isin. a")
!
! ! the size of the result is the size of a
! CALL OK(SIZE(a .isin. b) .EQ. 4_I4B, "size of the result")
!
! ! no common entry
! CALL OK(.NOT. ANY([1_I4B, 2_I4B] .isin. a), "no common entry")
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.isin.)
  MODULE PURE FUNCTION isin_1a(a, b) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1a

  MODULE PURE FUNCTION isin_1b(a, b) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1b

  MODULE PURE FUNCTION isin_1c(a, b) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1c

  MODULE PURE FUNCTION isin_1d(a, b) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1d
END INTERFACE OPERATOR(.isin.)

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if an integer is an entry of a vector.
!
! Description:
!   Returns `.TRUE.` if the scalar `a` is equal to at least one entry
!   of `b`.
!
!   Defined binary operators have the lowest precedence, so write
!   `.NOT. (a .in. b)` with parentheses.
!
! Arguments:
!   a : INTEGER - value to test
!   b : INTEGER, rank(1) - reference set, same kind as `a`
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if `a` is in `b`
!
! Formula:
!   $$\mathrm{ans} = \left( a \in b \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: OPERATOR(.in.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: b(4)
!
! b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
!
! CALL OK(12_I4B .in. b, "12 is in b")
! CALL OK(9_I4B .in. b, "9 is in b")
! CALL OK(.NOT. (13_I4B .in. b), "13 is not in b")
! CALL OK(.NOT. (-9_I4B .in. b), "-9 is not in b")
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.in.)
  MODULE PURE FUNCTION in_2a(a, b) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a
    INTEGER(INT8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2a

  MODULE PURE FUNCTION in_2b(a, b) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a
    INTEGER(INT16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2b

  MODULE PURE FUNCTION in_2c(a, b) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a
    INTEGER(INT32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2c

  MODULE PURE FUNCTION in_2d(a, b) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a
    INTEGER(INT64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2d
END INTERFACE OPERATOR(.in.)

!----------------------------------------------------------------------------
!                                           Operator(.isin.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if an integer is an entry of a vector.
!
! Description:
!   For a scalar `a`, `a .isin. b` is the same as `a .in. b`. It
!   returns a scalar logical, which is `.TRUE.` if `a` is equal to at
!   least one entry of `b`.
!
! Arguments:
!   a : INTEGER - value to test
!   b : INTEGER, rank(1) - reference set, same kind as `a`
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if `a` is in `b`
!
! Formula:
!   $$\mathrm{ans} = \left( a \in b \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: OPERATOR(.isin.)
! USE IntegerUtility, ONLY: OPERATOR(.in.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: b(4)
!
! b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
!
! ! for a scalar, .isin. returns a scalar logical
! CALL OK(12_I4B .isin. b, "12 is in b")
! CALL OK(.NOT. (13_I4B .isin. b), "13 is not in b")
!
! ! for a scalar, .isin. is the same as .in.
! CALL OK((10_I4B .isin. b) .EQV. (10_I4B .in. b), "same as .in.")
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.isin.)
  MODULE PROCEDURE in_2a, in_2b, in_2c, in_2d
END INTERFACE OPERATOR(.isin.)

!----------------------------------------------------------------------------
!                                                           RemoveDuplicates
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Removes repeated entries from an allocatable integer vector.
!
! Description:
!   Sorts `obj` in increasing order, removes the repeated entries, and
!   reallocates `obj` to the number of unique entries.
!
!   - If `obj` is not allocated, or has at most one entry, it is not
!     changed.
!   - Use `RemoveDuplicates_` to avoid the reallocation.
!
! Arguments:
!   obj : INTEGER, ALLOCATABLE, rank(1) - on entry the input vector;
!     on exit the sorted unique entries
!
! Formula:
!   $$\mathrm{obj} \leftarrow \mathrm{sort}\left(
!     \mathrm{unique}(\mathrm{obj}) \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: RemoveDuplicates
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! duplicates are removed and the result is sorted
! avec = [5_I4B, 4_I4B, 4_I4B, 4_I4B, 3_I4B, 3_I4B, 2_I4B, 2_I4B, &
!         1_I4B, 1_I4B, 1_I4B]
! CALL RemoveDuplicates(avec)
! CALL OK(SIZE(avec) .EQ. 5_I4B, "size after removing duplicates")
! CALL OK(ALL(avec .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "unique entries")
!
! ! no duplicates: the vector is only sorted
! avec = [3_I4B, 1_I4B, 2_I4B]
! CALL RemoveDuplicates(avec)
! CALL OK(ALL(avec .EQ. [1_I4B, 2_I4B, 3_I4B]), "no duplicates")
!
! ! all entries are the same
! avec = [7_I4B, 7_I4B, 7_I4B]
! CALL RemoveDuplicates(avec)
! CALL OK(SIZE(avec) .EQ. 1_I4B, "all entries are the same")
! CALL OK(avec(1) .EQ. 7_I4B, "the only entry")
!
! ! an unallocated vector is not changed
! DEALLOCATE (avec)
! CALL RemoveDuplicates(avec)
! CALL OK(.NOT. ALLOCATED(avec), "unallocated vector")
!
! END PROGRAM main
!```

INTERFACE RemoveDuplicates
  MODULE PURE SUBROUTINE RemoveDuplicates_1a(obj)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1a

  MODULE PURE SUBROUTINE RemoveDuplicates_1b(obj)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1b

  MODULE PURE SUBROUTINE RemoveDuplicates_1c(obj)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1c

  MODULE PURE SUBROUTINE RemoveDuplicates_1d(obj)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1d
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!                                                           RemoveDuplicate_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Removes repeated entries from an integer vector, no allocation.
!
! Description:
!   Moves the unique entries of `obj` to `obj(1:tsize)` in increasing
!   order. `obj` is not reallocated.
!
!   - If `isSorted` is `.FALSE.`, `obj` is sorted first.
!   - If `isSorted` is `.TRUE.`, `obj` is not sorted, and only
!     consecutive repeated entries are removed. So `obj` must really
!     be sorted in this case. This is not checked: for unsorted input
!     equal entries that are not next to each other are kept, and the
!     result is not in increasing order. For example `[3, 3, 1, 3]`
!     gives `obj(1:tsize) = [3, 1, 3]`.
!   - The entries `obj(tsize+1:)` are set to zero.
!   - If `SIZE(obj) <= 1`, `obj` is not changed and
!     `tsize = SIZE(obj)`.
!
! Arguments:
!   obj : INTEGER, rank(1) - on exit `obj(1:tsize)` has the unique
!     entries
!   tsize : INTEGER(I4B) - number of unique entries
!   isSorted : LOGICAL(LGT) - set to `.TRUE.` if `obj` is already
!     sorted
!
! Formula:
!   $$\mathrm{obj}(1{:}\mathrm{tsize}) = \mathrm{sort}\left(
!     \mathrm{unique}(\mathrm{obj}) \right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: RemoveDuplicates_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: avec(11), bvec(6)
! INTEGER(I4B) :: tsize
!
! ! unsorted vector: set isSorted to false
! avec = [5_I4B, 4_I4B, 4_I4B, 4_I4B, 3_I4B, 3_I4B, 2_I4B, 2_I4B, &
!         1_I4B, 1_I4B, 1_I4B]
! CALL RemoveDuplicates_(avec, tsize, .FALSE.)
! CALL OK(tsize .EQ. 5_I4B, "number of unique entries")
! CALL OK(ALL(avec(1:tsize) .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "unique entries")
! CALL OK(ALL(avec(tsize + 1:) .EQ. 0_I4B), "remaining entries are zero")
!
! ! sorted vector: set isSorted to true to skip sorting
! bvec = [1_I4B, 1_I4B, 2_I4B, 3_I4B, 3_I4B, 3_I4B]
! CALL RemoveDuplicates_(obj=bvec, tsize=tsize, isSorted=.TRUE.)
! CALL OK(tsize .EQ. 3_I4B, "sorted: number of unique entries")
! CALL OK(ALL(bvec(1:tsize) .EQ. [1_I4B, 2_I4B, 3_I4B]), &
!         "sorted: unique entries")
!
! END PROGRAM main
!```

INTERFACE RemoveDuplicates_
  MODULE PURE SUBROUTINE RemoveDuplicates_1a_(obj, tsize, isSorted)
    INTEGER(INT8), INTENT(INOUT) :: obj(:)
    !! obj(1:tsize) will have unique entries
    LOGICAL(LGT), INTENT(IN) :: isSorted
    !! if obj is sorted then set isSorted to true
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of unique entries found
  END SUBROUTINE RemoveDuplicates_1a_

  MODULE PURE SUBROUTINE RemoveDuplicates_1b_(obj, tsize, isSorted)
    INTEGER(INT16), INTENT(INOUT) :: obj(:)
    !! obj(1:tsize) will have unique entries
    LOGICAL(LGT), INTENT(IN) :: isSorted
    !! if obj is sorted then set isSorted to true
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of unique entries found
  END SUBROUTINE RemoveDuplicates_1b_

  MODULE PURE SUBROUTINE RemoveDuplicates_1c_(obj, tsize, isSorted)
    INTEGER(INT32), INTENT(INOUT) :: obj(:)
    !! obj(1:tsize) will have unique entries
    LOGICAL(LGT), INTENT(IN) :: isSorted
    !! if obj is sorted then set isSorted to true
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of unique entries found
  END SUBROUTINE RemoveDuplicates_1c_

  MODULE PURE SUBROUTINE RemoveDuplicates_1d_(obj, tsize, isSorted)
    INTEGER(INT64), INTENT(INOUT) :: obj(:)
    !! obj(1:tsize) will have unique entries
    LOGICAL(LGT), INTENT(IN) :: isSorted
    !! if obj is sorted then set isSorted to true
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of unique entries found
  END SUBROUTINE RemoveDuplicates_1d_
END INTERFACE RemoveDuplicates_

!----------------------------------------------------------------------------
!                                                                    Repeat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Repeats an integer vector several times.
!
! Description:
!   Returns `[val, val, ..., val]`, where `val` appears `rtimes`
!   times. The result has the same kind as `val`.
!
!   - If `rtimes <= 0` the result is an empty vector.
!   - This generic extends the intrinsic `REPEAT`, so
!     `REPEAT(string, ncopies)` still works when this module is used.
!
! Arguments:
!   val : INTEGER, rank(1) - vector to repeat
!   rtimes : INTEGER(I4B) - number of copies; `rtimes <= 0` gives an
!     empty vector
!
! Returns:
!   ans : INTEGER, rank(1) - repeated vector,
!     `SIZE(ans) = SIZE(val) * MAX(rtimes, 0)`
!
! Formula:
!   $$\mathrm{ans}_{(r-1)m + i} = \mathrm{val}_i, \quad
!     i = 1, \ldots, m, \quad r = 1, \ldots, \mathrm{rtimes},
!     \quad m = \mathrm{SIZE}(\mathrm{val})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: Repeat
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: val(3)
!
! val = [1_I4B, 2_I4B, 3_I4B]
!
! ! repeat three times
! CALL OK(SIZE(Repeat(val, 3_I4B)) .EQ. 9_I4B, "size of the result")
! CALL OK(ALL(Repeat(val, 3_I4B) .EQ. [val, val, val]), "repeat 3 times")
! CALL OK(ALL(Repeat(val, 2_I4B) .EQ. &
!             [1_I4B, 2_I4B, 3_I4B, 1_I4B, 2_I4B, 3_I4B]), &
!         "repeat 2 times")
!
! ! repeat once
! CALL OK(ALL(Repeat(val=val, rtimes=1_I4B) .EQ. val), "repeat once")
!
! ! rtimes <= 0 gives an empty vector
! CALL OK(SIZE(Repeat(val, 0_I4B)) .EQ. 0_I4B, "rtimes = 0")
! CALL OK(SIZE(Repeat(val, -2_I4B)) .EQ. 0_I4B, "rtimes < 0")
!
! ! the intrinsic REPEAT for strings is still available
! CALL OK(REPEAT("ab", 2) .EQ. "abab", "intrinsic REPEAT")
!
! END PROGRAM main
!```

INTERFACE Repeat
  MODULE PURE FUNCTION Repeat_1a(val, rtimes) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(INT8) :: ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1a

  MODULE PURE FUNCTION Repeat_1b(Val, rtimes) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(INT16) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1b

  MODULE PURE FUNCTION Repeat_1c(Val, rtimes) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(INT32) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1c

  MODULE PURE FUNCTION Repeat_1d(Val, rtimes) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(INT64) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1d
END INTERFACE Repeat

!----------------------------------------------------------------------------
!                                                                    Repeat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Repeats a real vector several times.
!
! Description:
!   Returns `[val, val, ..., val]`, where `val` appears `rtimes`
!   times. It is available for `REAL32` and `REAL64`. The result has
!   the same kind as `val`.
!
!   If `rtimes <= 0` the result is an empty vector.
!
! Arguments:
!   val : REAL, rank(1) - vector to repeat
!   rtimes : INTEGER(I4B) - number of copies; `rtimes <= 0` gives an
!     empty vector
!
! Returns:
!   ans : REAL, rank(1) - repeated vector,
!     `SIZE(ans) = SIZE(val) * MAX(rtimes, 0)`
!
! Formula:
!   $$\mathrm{ans}_{(r-1)m + i} = \mathrm{val}_i, \quad
!     i = 1, \ldots, m, \quad r = 1, \ldots, \mathrm{rtimes},
!     \quad m = \mathrm{SIZE}(\mathrm{val})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: Repeat
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: val(2), ans(6)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! val = [1.5_DFP, -2.5_DFP]
!
! ans = Repeat(val, 3_I4B)
! CALL OK(ALL(ABS(ans - [val, val, val]) .LT. tol), "repeat 3 times")
! CALL OK(ABS(ans(5) - 1.5_DFP) .LT. tol, "fifth entry")
! CALL OK(SIZE(Repeat(val, 4_I4B)) .EQ. 8_I4B, "size of the result")
!
! ! rtimes <= 0 gives an empty vector
! CALL OK(SIZE(Repeat(val, 0_I4B)) .EQ. 0_I4B, "rtimes = 0")
!
! END PROGRAM main
!```

INTERFACE Repeat
  MODULE PURE FUNCTION Repeat_1e(Val, rtimes) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    REAL(REAL32) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1e

  MODULE PURE FUNCTION Repeat_1f(Val, rtimes) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    REAL(REAL64) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1f
END INTERFACE Repeat

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Index of a value in an integer vector.
!
! Description:
!   Returns the index of the first entry of `obj` that is equal to
!   `val`, or zero if `val` is not in `obj`. It is available only for
!   `INTEGER(I4B)`. The result is the same as that of the vector
!   version `GetIndex(obj, val(:))` for one value.
!
! Arguments:
!   obj : INTEGER(I4B), rank(1) - vector to search
!   val : INTEGER(I4B) - value to find
!
! Returns:
!   ans : INTEGER(I4B) - index in `obj`, or zero if not found
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \min \{ i : \mathrm{obj}_i = \mathrm{val} \},
!       & \text{if val is in obj} \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: obj(4), obj2(3)
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! CALL OK(GetIndex(obj, 30_I4B) .EQ. 3_I4B, "index of 30")
! CALL OK(GetIndex(obj, 10_I4B) .EQ. 1_I4B, "index of 10")
! CALL OK(GetIndex(obj=obj, val=40_I4B) .EQ. 4_I4B, "index of 40")
!
! ! the first index is returned for a repeated value
! obj2 = [5_I4B, 7_I4B, 5_I4B]
! CALL OK(GetIndex(obj2, 5_I4B) .EQ. 1_I4B, "first index of 5")
!
! ! a value that is not in obj gives zero
! CALL OK(GetIndex(obj, 31_I4B) .EQ. 0_I4B, "31 is not found")
! CALL OK(GetIndex(obj, 0_I4B) .EQ. 0_I4B, "0 is not found")
! CALL OK(GetIndex(obj, 99_I4B) .EQ. 0_I4B, "99 is not found")
!
! ! the same result as the vector version
! CALL OK(ALL(GetIndex(obj, [30_I4B, 31_I4B]) .EQ. [3_I4B, 0_I4B]), &
!         "same as GetIndex(obj, val(:))")
!
! END PROGRAM main
!```

INTERFACE GetIndex
  MODULE PURE FUNCTION GetIndex1(obj, val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: val
    INTEGER(I4B) :: ans
  END FUNCTION GetIndex1
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Indices of several values in an integer vector.
!
! Description:
!   For each `val(j)`, `ans(j)` is the first index `i` such that
!   `obj(i) .EQ. val(j)`. If `val(j)` is not in `obj`, `ans(j)` is
!   zero. It is available only for `INTEGER(I4B)`.
!
! Arguments:
!   obj : INTEGER(I4B), rank(1) - vector to search
!   val : INTEGER(I4B), rank(1) - values to find
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(1) - indices in `obj`,
!     `SIZE(ans) = SIZE(val)`
!
! Formula:
!   $$\mathrm{ans}_j = \begin{cases}
!     \min \{ i : \mathrm{obj}_i = \mathrm{val}_j \}, &
!       \text{if } \mathrm{val}_j \in \mathrm{obj} \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: obj(4)
! INTEGER(I4B), ALLOCATABLE :: ans(:)
!
! obj = [10_I4B, 20_I4B, 30_I4B, 20_I4B]
!
! ! ans(j) is the first index of val(j) in obj, zero if not found
! ans = GetIndex(obj, [20_I4B, 40_I4B, 10_I4B])
! CALL OK(SIZE(ans) .EQ. 3_I4B, "size of the result")
! CALL OK(ALL(ans .EQ. [2_I4B, 0_I4B, 1_I4B]), "indices")
!
! ! no value is found
! ans = GetIndex(obj, [1_I4B, 2_I4B])
! CALL OK(ALL(ans .EQ. 0_I4B), "no value is found")
!
! END PROGRAM main
!```

INTERFACE GetIndex
  MODULE PURE FUNCTION GetIndex2(obj, Val) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: Val(:)
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION GetIndex2
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Returns one entry of an integer vector.
!
! Description:
!   Returns `val(indx)`. If `indx` is outside `1..SIZE(val)` the
!   result is zero.
!
! Arguments:
!   val : INTEGER, rank(1) - input vector
!   indx : INTEGER(I4B) - index of the entry
!
! Returns:
!   ans : INTEGER - the entry, same kind as `val`
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{val}_{\mathrm{indx}}, &
!       \text{if } 1 \le \mathrm{indx} \le \mathrm{SIZE}(\mathrm{val}) \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: Get
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: val(3)
!
! val = [10_I4B, 20_I4B, 30_I4B]
!
! CALL OK(Get(val, 1_I4B) .EQ. 10_I4B, "first entry")
! CALL OK(Get(val=val, indx=3_I4B) .EQ. 30_I4B, "last entry")
!
! ! indx outside 1..SIZE(val) returns zero
! CALL OK(Get(val, 4_I4B) .EQ. 0_I4B, "indx out of range")
! CALL OK(Get(val, 0_I4B) .EQ. 0_I4B, "indx = 0")
! CALL OK(Get(val, -1_I4B) .EQ. 0_I4B, "indx < 0")
!
! END PROGRAM main
!```

INTERFACE Get
  MODULE PURE FUNCTION Get1_Int8(val, indx) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT8) :: ans
  END FUNCTION Get1_Int8

  MODULE PURE FUNCTION Get1_Int16(val, indx) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT16) :: ans
  END FUNCTION Get1_Int16

  MODULE PURE FUNCTION Get1_Int32(val, indx) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT32) :: ans
  END FUNCTION Get1_Int32

  MODULE PURE FUNCTION Get1_Int64(val, indx) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT64) :: ans
  END FUNCTION Get1_Int64
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Returns several entries of an integer vector.
!
! Description:
!   Returns `val(indx)`, where `indx` is a vector of indices.
!
!   - Repeated indices are allowed.
!   - Every index should be between 1 and `SIZE(val)`. This is not
!     checked.
!
! Arguments:
!   val : INTEGER, rank(1) - input vector
!   indx : INTEGER(I4B), rank(1) - indices of the entries
!
! Returns:
!   ans : INTEGER, rank(1) - the entries, same kind as `val`,
!     `SIZE(ans) = SIZE(indx)`
!
! Formula:
!   $$\mathrm{ans}_k = \mathrm{val}_{\mathrm{indx}_k}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: Get
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: val(4)
!
! val = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! CALL OK(ALL(Get(val, [3_I4B, 1_I4B]) .EQ. [30_I4B, 10_I4B]), &
!         "entries 3 and 1")
!
! ! repeated indices are allowed
! CALL OK(ALL(Get(val, [2_I4B, 2_I4B]) .EQ. [20_I4B, 20_I4B]), &
!         "repeated indices")
!
! ! the size of the result is the size of indx
! CALL OK(SIZE(Get(val, [1_I4B, 2_I4B, 4_I4B])) .EQ. 3_I4B, &
!         "size of the result")
!
! END PROGRAM main
!```

INTERFACE Get
  MODULE PURE FUNCTION Get2_Int8(val, indx) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(INT8) :: ans(SIZE(indx))
  END FUNCTION Get2_Int8

  MODULE PURE FUNCTION Get2_Int16(val, indx) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(INT16) :: ans(SIZE(indx))
  END FUNCTION Get2_Int16

  MODULE PURE FUNCTION Get2_Int32(val, indx) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(INT32) :: ans(SIZE(indx))
  END FUNCTION Get2_Int32

  MODULE PURE FUNCTION Get2_Int64(val, indx) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(INT64) :: ans(SIZE(indx))
  END FUNCTION Get2_Int64
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Returns a strided section of an integer vector.
!
! Description:
!   Returns the section `val(istart:iend:stride)`.
!
!   - `stride` may be negative, but it should not be zero.
!   - `iend` need not be reached exactly.
!   - The section should lie inside `val`. This is not checked.
!
! Arguments:
!   val : INTEGER, rank(1) - input vector
!   istart : INTEGER(I4B) - first index
!   iend : INTEGER(I4B) - last index
!   stride : INTEGER(I4B) - step between indices, `stride /= 0`
!
! Returns:
!   ans : INTEGER, rank(1) - the section, same kind as `val`,
!     `SIZE(ans) = (iend - istart) / stride + 1`
!
! Formula:
!   $$\mathrm{ans}_k = \mathrm{val}_{\mathrm{istart}
!     + (k-1)\,\mathrm{stride}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: Get
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: val(6)
!
! val = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B, 60_I4B]
!
! ! Get(val, istart, iend, stride) is val(istart:iend:stride)
! CALL OK(ALL(Get(val, 1_I4B, 5_I4B, 2_I4B) .EQ. &
!             [10_I4B, 30_I4B, 50_I4B]), "val(1:5:2)")
! CALL OK(ALL(Get(val, 2_I4B, 6_I4B, 2_I4B) .EQ. &
!             [20_I4B, 40_I4B, 60_I4B]), "val(2:6:2)")
! CALL OK(ALL(Get(val, 2_I4B, 4_I4B, 1_I4B) .EQ. val(2:4)), "val(2:4)")
!
! ! iend need not be reached exactly
! CALL OK(SIZE(Get(val, 1_I4B, 6_I4B, 2_I4B)) .EQ. 3_I4B, "val(1:6:2)")
!
! ! negative stride
! CALL OK(ALL(Get(val, 6_I4B, 1_I4B, -2_I4B) .EQ. &
!             [60_I4B, 40_I4B, 20_I4B]), "val(6:1:-2)")
!
! END PROGRAM main
!```

INTERFACE Get
  MODULE PURE FUNCTION Get3_Int8(val, istart, iend, stride) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    INTEGER(INT8) :: ans(INT((iend - istart) / stride) + 1)
  END FUNCTION Get3_Int8

  MODULE PURE FUNCTION Get3_Int16(val, istart, iend, stride) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    INTEGER(INT16) :: ans(INT((iend - istart) / stride) + 1)
  END FUNCTION Get3_Int16

  MODULE PURE FUNCTION Get3_Int32(val, istart, iend, stride) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    INTEGER(INT32) :: ans(INT((iend - istart) / stride) + 1)
  END FUNCTION Get3_Int32

  MODULE PURE FUNCTION Get3_Int64(val, istart, iend, stride) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    INTEGER(INT64) :: ans(INT((iend - istart) / stride) + 1)
  END FUNCTION Get3_Int64
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                           GetIntersection
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Common entries of two integer vectors.
!
! Description:
!   Writes the entries which are in both `a` and `b` into
!   `c(1:tsize)`.
!
!   - The smaller vector is scanned: `a` if `SIZE(a) <= SIZE(b)`,
!     otherwise `b`. `c` keeps the order of the scanned vector, and a
!     repeated entry of the scanned vector is repeated in `c`.
!   - The vectors need not be sorted.
!   - `c` should have at least `MIN(SIZE(a), SIZE(b))` entries. This
!     is not checked.
!
! Arguments:
!   a : INTEGER, rank(1) - first vector
!   b : INTEGER, rank(1) - second vector, same kind as `a`
!   c : INTEGER, rank(1) - on exit `c(1:tsize)` has the common entries
!   tsize : INTEGER(I4B) - number of common entries written to `c`
!
! Formula:
!   $$c(1{:}\mathrm{tsize}) = \{ x \in s : x \in t \}, \quad
!     (s, t) = \begin{cases}
!       (a, b), & \mathrm{SIZE}(a) \le \mathrm{SIZE}(b) \\
!       (b, a), & \text{otherwise}
!     \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntegerUtility, ONLY: GetIntersection
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: c(5)
! INTEGER(I4B) :: tsize
!
! ! a is smaller: c follows the order of a
! CALL GetIntersection([1_I4B, 2_I4B, 3_I4B, 4_I4B], &
!                      [7_I4B, 4_I4B, 5_I4B, 6_I4B, 3_I4B], c, tsize)
! CALL OK(tsize .EQ. 2_I4B, "size of the intersection")
! CALL OK(ALL(c(1:tsize) .EQ. [3_I4B, 4_I4B]), "intersection")
!
! ! b is smaller: c follows the order of b
! CALL GetIntersection(a=[5_I4B, 1_I4B, 3_I4B, 9_I4B, 7_I4B], &
!                      b=[3_I4B, 1_I4B], c=c, tsize=tsize)
! CALL OK(tsize .EQ. 2_I4B, "size of the intersection (b smaller)")
! CALL OK(ALL(c(1:tsize) .EQ. [3_I4B, 1_I4B]), "intersection (b smaller)")
!
! ! no common entry
! CALL GetIntersection([1_I4B, 2_I4B], [3_I4B, 4_I4B], c, tsize)
! CALL OK(tsize .EQ. 0_I4B, "empty intersection")
!
! END PROGRAM main
!```

INTERFACE GetIntersection
  MODULE PURE SUBROUTINE GetIntersection1(a, b, c, tsize)
    INTEGER(INT8), INTENT(IN) :: a(:), b(:)
    INTEGER(INT8), INTENT(INOUT) :: c(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetIntersection1

  MODULE PURE SUBROUTINE GetIntersection2(a, b, c, tsize)
    INTEGER(INT16), INTENT(IN) :: a(:), b(:)
    INTEGER(INT16), INTENT(INOUT) :: c(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetIntersection2

  MODULE PURE SUBROUTINE GetIntersection3(a, b, c, tsize)
    INTEGER(INT32), INTENT(IN) :: a(:), b(:)
    INTEGER(INT32), INTENT(INOUT) :: c(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetIntersection3

  MODULE PURE SUBROUTINE GetIntersection4(a, b, c, tsize)
    INTEGER(INT64), INTENT(IN) :: a(:), b(:)
    INTEGER(INT64), INTENT(INOUT) :: c(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetIntersection4
END INTERFACE GetIntersection

!----------------------------------------------------------------------------
!                                                         Get1DIndexFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-14
! summary: 1D index of the entry (i, j) of a 2D Fortran array.
!
! Description:
!   Returns the position of `A(i, j)` in the column major storage of
!   an array `A(dim1, dim2)`, that is, the index of `A(i, j)` in
!   `RESHAPE(A, [dim1 * dim2])`.
!
!   - `dim2` is not used in the formula.
!   - The indices are not checked against the dimensions.
!
! Arguments:
!   i : INTEGER(I4B) - row index, `1 <= i <= dim1`
!   j : INTEGER(I4B) - column index, `1 <= j <= dim2`
!   dim1 : INTEGER(I4B) - first dimension of the array
!   dim2 : INTEGER(I4B) - second dimension of the array
!
! Returns:
!   ans : INTEGER(I4B) - 1D index
!
! Formula:
!   $$\mathrm{ans} = (j - 1)\,\mathrm{dim1} + i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntegerUtility, ONLY: Get1DIndexFortran
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(2, 3), i, j
! LOGICAL(LGT) :: isok
!
! ! a(i, j) stores its own 1D (column major) index
! a = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], [2_I4B, 3_I4B])
!
! CALL OK(Get1DIndexFortran(1_I4B, 1_I4B, 2_I4B, 3_I4B) .EQ. 1_I4B, &
!         "(1, 1)")
! CALL OK(Get1DIndexFortran(1_I4B, 2_I4B, 2_I4B, 3_I4B) .EQ. 3_I4B, &
!         "(1, 2)")
! CALL OK(Get1DIndexFortran(i=2_I4B, j=3_I4B, dim1=2_I4B, &
!                           dim2=3_I4B) .EQ. 6_I4B, "(2, 3)")
!
! isok = .TRUE.
! DO j = 1, 3
!   DO i = 1, 2
!     isok = isok .AND. (Get1DIndexFortran(i, j, 2_I4B, 3_I4B) .EQ. a(i, j))
!   END DO
! END DO
! CALL OK(isok, "same as column major storage")
!
! END PROGRAM main
!```

INTERFACE Get1DIndexFortran
  MODULE PURE FUNCTION Get1DIndexFrom2DFortranIndex(i, j, dim1, dim2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B), INTENT(IN) :: j
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B) :: ans
  END FUNCTION Get1DIndexFrom2DFortranIndex
END INTERFACE Get1DIndexFortran

!----------------------------------------------------------------------------
!                                                         Get1DIndexFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-14
! summary: 1D index of the entry (i, j, k) of a 3D Fortran array.
!
! Description:
!   Returns the position of `A(i, j, k)` in the column major storage
!   of an array `A(dim1, dim2, dim3)`.
!
!   - `dim3` is not used in the formula.
!   - The indices are not checked against the dimensions.
!
! Arguments:
!   i : INTEGER(I4B) - first index, `1 <= i <= dim1`
!   j : INTEGER(I4B) - second index, `1 <= j <= dim2`
!   k : INTEGER(I4B) - third index, `1 <= k <= dim3`
!   dim1 : INTEGER(I4B) - first dimension of the array
!   dim2 : INTEGER(I4B) - second dimension of the array
!   dim3 : INTEGER(I4B) - third dimension of the array
!
! Returns:
!   ans : INTEGER(I4B) - 1D index
!
! Formula:
!   $$\mathrm{ans} = (k - 1)\,\mathrm{dim1}\,\mathrm{dim2}
!     + (j - 1)\,\mathrm{dim1} + i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntegerUtility, ONLY: Get1DIndexFortran
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: i, j, k, want
! LOGICAL(LGT) :: isok
!
! CALL OK(Get1DIndexFortran(1_I4B, 1_I4B, 2_I4B, 2_I4B, 3_I4B, 4_I4B) &
!         .EQ. 7_I4B, "(1, 1, 2)")
! CALL OK(Get1DIndexFortran(2_I4B, 3_I4B, 4_I4B, 2_I4B, 3_I4B, 4_I4B) &
!         .EQ. 24_I4B, "(2, 3, 4)")
!
! ! the index increases by one when i runs fastest
! isok = .TRUE.
! want = 0_I4B
! DO k = 1, 4
!   DO j = 1, 3
!     DO i = 1, 2
!       want = want + 1_I4B
!       isok = isok .AND. &
!              (Get1DIndexFortran(i, j, k, 2_I4B, 3_I4B, 4_I4B) .EQ. want)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "same as column major storage")
!
! END PROGRAM main
!```

INTERFACE Get1DIndexFortran
  MODULE PURE FUNCTION Get1DIndexFrom3DFortranIndex(i, j, k, dim1, dim2, &
                                                    dim3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B), INTENT(IN) :: j
    INTEGER(I4B), INTENT(IN) :: k
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B) :: ans
  END FUNCTION Get1DIndexFrom3DFortranIndex
END INTERFACE Get1DIndexFortran

!----------------------------------------------------------------------------
!                                                         Get1DIndexFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-14
! summary: 1D index of the entry (i, j, k, l) of a 4D Fortran array.
!
! Description:
!   Returns the position of `A(i, j, k, l)` in the column major
!   storage of an array `A(dim1, dim2, dim3, dim4)`.
!
!   - `dim4` is not used in the formula.
!   - The indices are not checked against the dimensions.
!
! Arguments:
!   i : INTEGER(I4B) - first index, `1 <= i <= dim1`
!   j : INTEGER(I4B) - second index, `1 <= j <= dim2`
!   k : INTEGER(I4B) - third index, `1 <= k <= dim3`
!   l : INTEGER(I4B) - fourth index, `1 <= l <= dim4`
!   dim1 : INTEGER(I4B) - first dimension of the array
!   dim2 : INTEGER(I4B) - second dimension of the array
!   dim3 : INTEGER(I4B) - third dimension of the array
!   dim4 : INTEGER(I4B) - fourth dimension of the array
!
! Returns:
!   ans : INTEGER(I4B) - 1D index
!
! Formula:
!   $$\mathrm{ans} = (l - 1)\,\mathrm{dim1}\,\mathrm{dim2}\,\mathrm{dim3}
!     + (k - 1)\,\mathrm{dim1}\,\mathrm{dim2}
!     + (j - 1)\,\mathrm{dim1} + i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntegerUtility, ONLY: Get1DIndexFortran
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: i, j, k, l, want
! LOGICAL(LGT) :: isok
!
! CALL OK(Get1DIndexFortran(1_I4B, 1_I4B, 1_I4B, 2_I4B, &
!                           2_I4B, 3_I4B, 4_I4B, 2_I4B) .EQ. 25_I4B, &
!         "(1, 1, 1, 2)")
! CALL OK(Get1DIndexFortran(2_I4B, 3_I4B, 4_I4B, 2_I4B, &
!                           2_I4B, 3_I4B, 4_I4B, 2_I4B) .EQ. 48_I4B, &
!         "(2, 3, 4, 2)")
!
! ! the index increases by one when i runs fastest
! isok = .TRUE.
! want = 0_I4B
! DO l = 1, 2
!   DO k = 1, 4
!     DO j = 1, 3
!       DO i = 1, 2
!         want = want + 1_I4B
!         isok = isok .AND. (Get1DIndexFortran(i, j, k, l, 2_I4B, 3_I4B, &
!                                              4_I4B, 2_I4B) .EQ. want)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "same as column major storage")
!
! END PROGRAM main
!```

INTERFACE Get1DIndexFortran
  MODULE PURE FUNCTION Get1DIndexFrom4DFortranIndex(i, j, k, l, dim1, dim2, &
                                                    dim3, dim4) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B), INTENT(IN) :: j
    INTEGER(I4B), INTENT(IN) :: k
    INTEGER(I4B), INTENT(IN) :: l
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(I4B) :: ans
  END FUNCTION Get1DIndexFrom4DFortranIndex
END INTERFACE Get1DIndexFortran

END MODULE IntegerUtility
