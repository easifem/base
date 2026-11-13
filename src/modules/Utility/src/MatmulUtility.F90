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
! date: 2021-04-03
! summary: Contraction of arrays of rank up to four.
!
! Description:
!   This module contains the generic function `Matmul` and the generic
!   subroutine `Matmul_`. They contract the last index of `a1` with the
!   first index of `a2`, so the rank of the result is
!   `RANK(a1) + RANK(a2) - 2`.
!
!   The following combinations of ranks are available:
!
!   - rank-4 `a1` with rank-1, 2, 3 or 4 `a2`,
!   - rank-3 `a1` with rank-1, 2, 3 or 4 `a2`,
!   - rank-2 `a1` with rank-3 or 4 `a2`,
!   - rank-1 `a1` with rank-1, 3 or 4 `a2`.
!
!   The generic name `Matmul` extends the intrinsic `MATMUL`, so the
!   rank-1 and rank-2 combinations of the intrinsic are still available.
!
!   `Matmul_` does the same work as `Matmul`, but it writes the result
!   into an array given by the caller and returns the size of the
!   written part. It does not allocate memory.
!
!   All arrays are `REAL(DFP)`. The result is the same as the intrinsic
!   `MATMUL` on reshaped arrays:
!
!```fortran
! ans = RESHAPE(MATMUL(RESHAPE(a1, [P, L]), RESHAPE(a2, [L, Q])), &
!               SHAPE(ans))
!```
!
!   where `L` is the size of the contracted index, `P` is the product
!   of the other extents of `a1`, and `Q` is the product of the other
!   extents of `a2`.
!
!   The contracted sizes `n = SIZE(a1, RANK(a1))` and `m = SIZE(a2, 1)`
!   should be equal. This is not checked: every combination sums over
!   the first `MIN(n, m)` indices only, so a mismatch gives a shorter
!   sum and no error.

MODULE MatmulUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: Matmul
PUBLIC :: Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a vector.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 3.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!
! Returns:
!   ans : REAL(DFP), rank(3) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3))`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 2, 4), a2(4)
! REAL(DFP) :: ans(2, 3, 2)
! REAL(DFP) :: ref(2, 3, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 48)], &
!              [2_I4B, 3_I4B, 2_I4B, 4_I4B])
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [12_I4B, 4_I4B]), a2), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 125.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 180.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r4_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3))
  END FUNCTION matmul_r4_r1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a vector without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-4 array and a vector,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 3)`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 2, 4), a2(4)
! REAL(DFP) :: ans(3, 4, 3)
! REAL(DFP) :: ref(2, 3, 2)
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 48)], &
!              [2_I4B, 3_I4B, 2_I4B, 4_I4B])
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 3, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 125.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 180.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r4_r1_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r4_r1_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-2 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 4.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(2) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!
! Returns:
!   ans : REAL(DFP), rank(4) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), SIZE(a2, 2))`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lm}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 2, 3), a2(3, 2)
! REAL(DFP) :: ans(2, 3, 2, 2)
! REAL(DFP) :: ref(2, 3, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 36)], &
!              [2_I4B, 3_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 6)], &
!              [3_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [12_I4B, 3_I4B]), a2), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 51.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 2) - 192.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r4_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), SIZE(a2, 2))
  END FUNCTION matmul_r4_r2
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-2 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-4 array and a rank-2 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(2) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 3)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lm}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 2, 3), a2(3, 2)
! REAL(DFP) :: ans(3, 4, 3, 3)
! REAL(DFP) :: ref(2, 3, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 36)], &
!              [2_I4B, 3_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 6)], &
!              [3_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 3, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 51.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 2) - 192.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r4_r2_(a1, a2, ans, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r4_r2_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-3 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 5.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!
! Returns:
!   ans : REAL(DFP), rank(5) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), SIZE(a2, 2), SIZE(a2, 3))`
!
! Formula:
!   $$\mathrm{ans}_{ijkmn} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lmn}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 2, 2, 3), a2(3, 2, 2)
! REAL(DFP) :: ans(2, 2, 2, 2, 2)
! REAL(DFP) :: ref(2, 2, 2, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 2_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 12)], &
!              [3_I4B, 2_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [8_I4B, 3_I4B]), &
!                      RESHAPE(a2, [3_I4B, 4_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 35.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2, 2) - 272.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 2, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r4_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), &
      & SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION matmul_r4_r3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-3 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-4 array and a rank-3 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!   ans : REAL(DFP), rank(5), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 3)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim5 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!
! Formula:
!   $$\mathrm{ans}_{ijkmn} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lmn}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 2, 2, 3), a2(3, 2, 2)
! REAL(DFP) :: ans(3, 3, 3, 3, 3)
! REAL(DFP) :: ref(2, 2, 2, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 2_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 12)], &
!              [3_I4B, 2_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4, dim5)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4, dim5] .EQ. [2, 2, 2, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5) &
!             - ref) .LT. tol), "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 35.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2, 2) - 272.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r4_r3_(a1, a2, ans, dim1, dim2, dim3, dim4, &
                                       dim5)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5
  END SUBROUTINE matmul_r4_r3_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-4 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 6.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!
! Returns:
!   ans : REAL(DFP), rank(6) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), SIZE(a2, 2), SIZE(a2, 3),
!       SIZE(a2, 4))`
!
! Formula:
!   $$\mathrm{ans}_{ijkmno} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lmno}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 2, 2, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(2, 2, 2, 2, 2, 2)
! REAL(DFP) :: ref(2, 2, 2, 2, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 2_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [8_I4B, 3_I4B]), &
!                      RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 35.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2, 2, 2) - 560.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 2, 2, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r4_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), &
      & SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION matmul_r4_r4
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-4 array with a rank-4 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-4 array and a rank-4 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5, 1:dim6)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 4)`
!   ans : REAL(DFP), rank(6), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 3)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim5 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!   dim6 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 4)`
!
! Formula:
!   $$\mathrm{ans}_{ijkmno} = \sum_{l=1}^{L} a1_{ijkl}\, a2_{lmno}$$
!
!   where $L$ is `SIZE(a1, 4)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 2, 2, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(3, 3, 3, 3, 3, 3)
! REAL(DFP) :: ref(2, 2, 2, 2, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5, dim6
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 2_I4B, 2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4, dim5, dim6)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4, dim5, dim6] .EQ. [2, 2, 2, 2, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5, 1:dim6) &
!             - ref) .LT. tol), "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 35.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2, 2, 2) - 560.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r4_r4_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4, dim5, dim6)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5, dim6
  END SUBROUTINE matmul_r4_r4_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a vector.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 2.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!
! Returns:
!   ans : REAL(DFP), rank(2) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2))`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \sum_{l=1}^{L} a1_{ijl}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4)
! REAL(DFP) :: ans(2, 3)
! REAL(DFP) :: ref(2, 3)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [6_I4B, 4_I4B]), a2), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3) - 90.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r3_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION matmul_r3_r1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a vector without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-3 array and a vector,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:nrow, 1:ncol)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   nrow : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   ncol : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \sum_{l=1}^{L} a1_{ijl}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4)
! REAL(DFP) :: ans(3, 4)
! REAL(DFP) :: ref(2, 3)
! INTEGER(I4B) :: nrow, ncol
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, nrow, ncol)
!
! ! the size of the written part
! CALL OK(ALL([nrow, ncol] .EQ. [2, 3]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3) - 90.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r3_r1_(a1, a2, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE matmul_r3_r1_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-2 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 3.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(2) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!
! Returns:
!   ans : REAL(DFP), rank(3) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a2, 2))`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lk}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4, 2)
! REAL(DFP) :: ans(2, 3, 2)
! REAL(DFP) :: ref(2, 3, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 8)], &
!              [4_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [6_I4B, 4_I4B]), a2), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 210.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r3_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
                     SIZE(a2, 2))
  END FUNCTION matmul_r3_r2
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-2 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-3 array and a rank-2 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(2) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lk}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4, 2)
! REAL(DFP) :: ans(3, 4, 3)
! REAL(DFP) :: ref(2, 3, 2)
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 8)], &
!              [4_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 3, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 210.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r3_r2_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r3_r2_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-3 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 4.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!
! Returns:
!   ans : REAL(DFP), rank(4) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a2, 2), SIZE(a2, 3))`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lkm}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4, 2, 3)
! REAL(DFP) :: ans(2, 3, 2, 3)
! REAL(DFP) :: ref(2, 3, 2, 3)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [4_I4B, 2_I4B, 3_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [6_I4B, 4_I4B]), &
!                      RESHAPE(a2, [4_I4B, 6_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 690.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3, 2, 3]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r3_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
                     SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION matmul_r3_r3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-3 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-3 array and a rank-3 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lkm}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 4), a2(4, 2, 3)
! REAL(DFP) :: ans(3, 4, 3, 4)
! REAL(DFP) :: ref(2, 3, 2, 3)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
!              [2_I4B, 3_I4B, 4_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [4_I4B, 2_I4B, 3_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 3, 2, 3]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 65.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 690.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r3_r3_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r3_r3_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-4 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 5.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!
! Returns:
!   ans : REAL(DFP), rank(5) - shape
!     `(SIZE(a1, 1), SIZE(a1, 2), SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))`
!
! Formula:
!   $$\mathrm{ans}_{ijkmn} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lkmn}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(2, 3, 2, 2, 2)
! REAL(DFP) :: ref(2, 3, 2, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 18)], &
!              [2_I4B, 3_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(RESHAPE(a1, [6_I4B, 3_I4B]), &
!                      RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 27.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 2, 2) - 420.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3, 2, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r3_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
                     SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION matmul_r3_r4
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-3 array with a rank-4 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-3 array and a rank-4 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 3)`
!   ans : REAL(DFP), rank(5), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!   dim5 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 4)`
!
! Formula:
!   $$\mathrm{ans}_{ijkmn} = \sum_{l=1}^{L} a1_{ijl}\, a2_{lkmn}$$
!
!   where $L$ is `SIZE(a1, 3)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(3, 4, 3, 3, 3)
! REAL(DFP) :: ref(2, 3, 2, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 18)], &
!              [2_I4B, 3_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4, dim5)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4, dim5] .EQ. [2, 3, 2, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4, 1:dim5) &
!             - ref) .LT. tol), "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 27.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 3, 2, 2, 2) - 420.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r3_r4_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4, dim5)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5
  END SUBROUTINE matmul_r3_r4_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-2 array with a rank-3 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 3.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 2)`
!
! Returns:
!   ans : REAL(DFP), rank(3) - shape
!     `(SIZE(a1, 1), SIZE(a2, 2), SIZE(a2, 3))`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{il}\, a2_{ljk}$$
!
!   where $L$ is `SIZE(a1, 2)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3), a2(3, 2, 4)
! REAL(DFP) :: ans(2, 2, 4)
! REAL(DFP) :: ref(2, 2, 4)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 6)], &
!              [2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 4_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(a1, RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 11.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 4) - 140.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 4]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r2_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION matmul_r2_r3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-2 array with a rank-3 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-2 array and a rank-3 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 2)`
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{il}\, a2_{ljk}$$
!
!   where $L$ is `SIZE(a1, 2)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3), a2(3, 2, 4)
! REAL(DFP) :: ans(3, 3, 5)
! REAL(DFP) :: ref(2, 2, 4)
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 6)], &
!              [2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 4_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 4]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 11.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 4) - 140.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r2_r3_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r2_r3_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-2 array with a rank-4 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 4.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 2)`
!
! Returns:
!   ans : REAL(DFP), rank(4) - shape
!     `(SIZE(a1, 1), SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{il}\, a2_{ljkm}$$
!
!   where $L$ is `SIZE(a1, 2)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(2, 2, 2, 2)
! REAL(DFP) :: ref(2, 2, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 6)], &
!              [2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(a1, RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 11.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2) - 140.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r2_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a2, 2), &
      & SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION matmul_r2_r4
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a rank-2 array with a rank-4 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a rank-2 array and a rank-4 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1, 2)`
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a1, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 4)`
!
! Formula:
!   $$\mathrm{ans}_{ijkm} = \sum_{l=1}^{L} a1_{il}\, a2_{ljkm}$$
!
!   where $L$ is `SIZE(a1, 2)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(2, 3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(3, 3, 3, 3)
! REAL(DFP) :: ref(2, 2, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii
!
! a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 6)], &
!              [2_I4B, 3_I4B])
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3, dim4)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 11.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2, 2) - 140.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r2_r4_(a1, a2, ans, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r2_r4_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a vector.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result is a scalar, the dot product of `a1` and `a2`.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!
! Returns:
!   ans : REAL(DFP) - scalar
!
! Formula:
!   $$\mathrm{ans} = \sum_{l=1}^{L} a1_{l}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3)
! REAL(DFP) :: ans
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 3)]
!
! ans = Matmul(a1, a2)
!
! ! a1 . a2 = 0.5 * (1 + 4 + 9)
! CALL OK(ABS(ans - 7.0_DFP) .LT. tol, "value of the result")
! CALL OK(ABS(ans - DOT_PRODUCT(a1, a2)) .LT. tol, &
!         "same as DOT_PRODUCT")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r1_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans
  END FUNCTION matmul_r1_r1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a vector without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a vector and a vector,
!   but the result is written into `ans`.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(1) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!   ans : REAL(DFP), INTENT(INOUT) - result
!
! Formula:
!   $$\mathrm{ans} = \sum_{l=1}^{L} a1_{l}\, a2_{l}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3)
! REAL(DFP) :: ans
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 3)]
!
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans)
!
! ! a1 . a2 = 0.5 * (1 + 4 + 9)
! CALL OK(ABS(ans - 7.0_DFP) .LT. tol, "value of the result")
! CALL OK(ABS(ans - DOT_PRODUCT(a1, a2)) .LT. tol, &
!         "same as DOT_PRODUCT")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r1_r1_(a1, a2, ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans
  END SUBROUTINE matmul_r1_r1_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a rank-3 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 2.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!
! Returns:
!   ans : REAL(DFP), rank(2) - shape
!     `(SIZE(a2, 2), SIZE(a2, 3))`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \sum_{l=1}^{L} a1_{l}\, a2_{lij}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3, 2, 4)
! REAL(DFP) :: ans(2, 4)
! REAL(DFP) :: ref(2, 4)
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 4_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(a1, RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1) - 7.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 4) - 70.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 4]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r1_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION matmul_r1_r3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a rank-3 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a vector and a rank-3 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:nrow, 1:ncol)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(3) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   nrow : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   ncol : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \sum_{l=1}^{L} a1_{l}\, a2_{lij}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3, 2, 4)
! REAL(DFP) :: ans(3, 5)
! REAL(DFP) :: ref(2, 4)
! INTEGER(I4B) :: nrow, ncol
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 4_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, nrow, ncol)
!
! ! the size of the written part
! CALL OK(ALL([nrow, ncol] .EQ. [2, 4]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1) - 7.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 4) - 70.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r1_r3_(a1, a2, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE matmul_r1_r3_
END INTERFACE Matmul_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a rank-4 array.
!
! Description:
!   Contracts the last index of `a1` with the first index of `a2`.
!   The result has rank 3.
!
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!
! Returns:
!   ans : REAL(DFP), rank(3) - shape
!     `(SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{l}\, a2_{lijk}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(2, 2, 2)
! REAL(DFP) :: ref(2, 2, 2)
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ans = Matmul(a1, a2)
!
! ! reference: contract the last index of a1 with the first
! ! index of a2 by using the intrinsic MATMUL on reshaped arrays
! ref = RESHAPE(MATMUL(a1, RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
! CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 7.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2) - 70.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the shape of the result
! CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 2]), &
!         "shape of the result")
!
! END PROGRAM main
!```

INTERFACE Matmul
  MODULE PURE FUNCTION matmul_r1_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION matmul_r1_r4
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contracts a vector with a rank-4 array without allocation.
!
! Description:
!   Same as `Matmul(a1, a2)` for a vector and a rank-4 array,
!   but the result is written into `ans`.
!
!   - `ans` should be allocated by the caller, and it can be larger
!     than the result.
!   - Only `ans(1:dim1, 1:dim2, 1:dim3)` is written.
!     The rest of `ans` is not changed.
!   - The sizes of the contracted indices should be equal. This is
!     not checked: if they differ, only the first `MIN(n, m)` terms
!     are summed, with `n = SIZE(a1, RANK(a1))` and
!     `m = SIZE(a2, 1)`, and no error is raised.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - first array, the last index is contracted
!   a2 : REAL(DFP), rank(4) - second array, the first index is contracted,
!     `SIZE(a2, 1) = SIZE(a1)`
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - result,
!     each extent of `ans` should be at least the matching extent
!     returned below
!   dim1 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 2)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 3)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `SIZE(a2, 4)`
!
! Formula:
!   $$\mathrm{ans}_{ijk} = \sum_{l=1}^{L} a1_{l}\, a2_{lijk}$$
!
!   where $L$ is `SIZE(a1)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatmulUtility, ONLY: Matmul
! USE MatmulUtility, ONLY: Matmul_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(3), a2(3, 2, 2, 2)
! REAL(DFP) :: ans(3, 3, 3)
! REAL(DFP) :: ref(2, 2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii
!
! a1 = [(REAL(ii, DFP), ii = 1, 3)]
! a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
!              [3_I4B, 2_I4B, 2_I4B, 2_I4B])
!
! ! ans is larger than needed, only a part of it is written
! ans = 0.0_DFP
! CALL Matmul_(a1, a2, ans, dim1, dim2, dim3)
!
! ! the size of the written part
! CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), &
!         "size of the result")
!
! ! same as the function version
! ref = Matmul(a1, a2)
! CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3) - ref) .LT. tol), &
!         "same as Matmul")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 7.0_DFP) .LT. tol, &
!         "first entry")
! CALL OK(ABS(ans(2, 2, 2) - 70.0_DFP) .LT. tol, &
!         "last entry")
!
! ! the rest of ans is not changed
! CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")
!
! END PROGRAM main
!```

INTERFACE Matmul_
  MODULE PURE SUBROUTINE matmul_r1_r4_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r1_r4_
END INTERFACE Matmul_

END MODULE MatmulUtility
