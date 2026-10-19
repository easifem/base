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
! date: 2021-03-22
! summary: Swap two variables, or permute the indices of an array.
!
! Description:
!   This module contains two generic subroutines.
!
!   `Swap(a, b)` exchanges the values of `a` and `b`. It is available
!   for scalars, vectors and matrices of integer (`INT8`, `INT16`,
!   `INT32`, `INT64`), real (`REAL32`, `REAL64`) and complex (`DFPC`)
!   type.
!
!   `Swap(a, b, mask)` exchanges the values only where `mask` is
!   true. It is available for integer and real scalars, vectors and
!   matrices.
!
!   `Swap(a, b, i1, i2, ...)` returns in the allocatable array `a` the
!   array `b` with its indices permuted. Dimension `k` of `a` runs
!   along dimension `ik` of `b`. For a rank-2 array `(i1, i2) = (2, 1)`
!   gives the transpose. It is available for real arrays of rank 2, 3
!   and 4.
!
!   `Swap_(a, b, i1, i2, ...)` does the same without allocating `a`.
!
!   Notes:
!
!   - When the code is built with `USE_BLAS95`, the real and complex
!     vector versions of this module are not compiled. The generic
!     `Swap` then also contains `SWAP` of `F95_BLAS`, which does the
!     same.
!   - The `INT128` versions are compiled only with `USE_Int128`.

MODULE SwapUtility
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: DFPC
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B

#ifdef USE_BLAS95
USE F95_BLAS, ONLY: SWAP
#endif

IMPLICIT NONE

PRIVATE

PUBLIC :: Swap
PUBLIC :: Swap_

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Swap two scalars.
!
! Description:
!   Exchanges the values of the scalars `a` and `b`. It is available
!   for integer (`INT8`, `INT16`, `INT32`, `INT64`), real (`REAL32`,
!   `REAL64`) and complex (`DFPC`) scalars. `a` and `b` must have the
!   same type and kind.
!
! Arguments:
!   a : INTEGER | REAL | COMPLEX, INTENT(INOUT) - on return, the old `b`
!   b : INTEGER | REAL | COMPLEX, INTENT(INOUT) - on return, the old `a`
!
! Returns:
!   The values of `a` and `b` are exchanged in place.
!
! Formula:
!   $$(a, b) \leftarrow (b, a)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DFPC
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia, ib
! REAL(DFP) :: ra, rb
! COMPLEX(DFPC) :: ca, cb
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! integer scalars
! ia = 3_I4B
! ib = 4_I4B
! CALL Swap(ia, ib)
! CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "swap integers")
!
! ! swapping twice gives back the input
! CALL Swap(ia, ib)
! CALL OK(ia .EQ. 3_I4B .AND. ib .EQ. 4_I4B, "swap integers twice")
!
! ! real scalars
! ra = 1.5_DFP
! rb = -2.5_DFP
! CALL Swap(ra, rb)
! CALL OK(ABS(ra + 2.5_DFP) .LT. tol .AND. ABS(rb - 1.5_DFP) .LT. tol, &
!         "swap reals")
!
! ! equal values stay equal
! ra = 7.0_DFP
! rb = 7.0_DFP
! CALL Swap(ra, rb)
! CALL OK(ABS(ra - 7.0_DFP) .LT. tol .AND. ABS(rb - 7.0_DFP) .LT. tol, &
!         "swap equal reals")
!
! ! complex scalars
! ca = (1.0_DFP, 2.0_DFP)
! cb = (3.0_DFP, -4.0_DFP)
! CALL Swap(ca, cb)
! CALL OK(ABS(ca - (3.0_DFP, -4.0_DFP)) .LT. tol, "swap complex: a")
! CALL OK(ABS(cb - (1.0_DFP, 2.0_DFP)) .LT. tol, "swap complex: b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int8

  MODULE PURE SUBROUTINE Swap_Int16(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int16

  MODULE PURE SUBROUTINE Swap_Int32(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int32

  MODULE PURE SUBROUTINE Swap_Int64(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int64

  MODULE PURE SUBROUTINE Swap_r32(a, b)
    REAL(REAL32), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_r32

  MODULE PURE SUBROUTINE Swap_r64(a, b)
    REAL(REAL64), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_r64

  MODULE PURE SUBROUTINE Swap_c(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_c
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Swap two vectors.
!
! Description:
!   Exchanges the entries of the vectors `a` and `b`. It is available
!   for integer (`INT8`, `INT16`, `INT32`, `INT64`), real (`REAL32`,
!   `REAL64`) and complex (`DFPC`) vectors.
!
!   - `a` and `b` should have the same size. If they differ, only the
!     first `MIN(SIZE(a), SIZE(b))` entries are exchanged.
!   - With `USE_BLAS95` the real and complex versions of this module
!     are not compiled, and `SWAP` of `F95_BLAS` is called instead.
!     The result is the same.
!
! Arguments:
!   a : INTEGER | REAL | COMPLEX, rank(1), INTENT(INOUT) - on return,
!     the old `b`
!   b : INTEGER | REAL | COMPLEX, rank(1), INTENT(INOUT) - on return,
!     the old `a`, normally `SIZE(b) = SIZE(a)`
!
! Returns:
!   The entries of `a` and `b` are exchanged in place.
!
! Formula:
!   $$(a_i, b_i) \leftarrow (b_i, a_i), \quad i = 1, \dots, n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DFPC
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia(4), ib(4)
! REAL(DFP) :: ra(3), rb(3)
! COMPLEX(DFPC) :: ca(2), cb(2)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! integer vectors
! ia = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
! ib = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
! CALL Swap(ia, ib)
! CALL OK(ALL(ia .EQ. [10_I4B, 20_I4B, 30_I4B, 40_I4B]), "integer: a")
! CALL OK(ALL(ib .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "integer: b")
!
! ! array sections
! CALL Swap(ia(1:2), ib(3:4))
! CALL OK(ALL(ia .EQ. [3_I4B, 4_I4B, 30_I4B, 40_I4B]), "section: a")
! CALL OK(ALL(ib .EQ. [1_I4B, 2_I4B, 10_I4B, 20_I4B]), "section: b")
!
! ! real vectors
! ra = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! rb = [-1.0_DFP, -2.0_DFP, -3.0_DFP]
! CALL Swap(ra, rb)
! CALL OK(ALL(ABS(ra - [-1.0_DFP, -2.0_DFP, -3.0_DFP]) .LT. tol), &
!         "real: a")
! CALL OK(ALL(ABS(rb - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), "real: b")
!
! ! complex vectors
! ca = [(1.0_DFP, 1.0_DFP), (2.0_DFP, 2.0_DFP)]
! cb = [(0.0_DFP, -1.0_DFP), (5.0_DFP, 0.0_DFP)]
! CALL Swap(ca, cb)
! CALL OK(ABS(ca(1) - (0.0_DFP, -1.0_DFP)) .LT. tol .AND. &
!         ABS(ca(2) - (5.0_DFP, 0.0_DFP)) .LT. tol, "complex: a")
! CALL OK(ABS(cb(1) - (1.0_DFP, 1.0_DFP)) .LT. tol .AND. &
!         ABS(cb(2) - (2.0_DFP, 2.0_DFP)) .LT. tol, "complex: b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8v(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int8v

  MODULE PURE SUBROUTINE Swap_Int16v(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int16v

  MODULE PURE SUBROUTINE Swap_Int32v(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int32v

  MODULE PURE SUBROUTINE Swap_Int64v(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int64v

#ifdef USE_Int128
  MODULE PURE SUBROUTINE Swap_Int128v(a, b)
    INTEGER(INT128), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int128v
#endif

#ifndef USE_BLAS95
  MODULE PURE SUBROUTINE Swap_r32v(a, b)
    REAL(REAL32), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_r32v

  MODULE PURE SUBROUTINE Swap_r64v(a, b)
    REAL(REAL64), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_r64v

  MODULE PURE SUBROUTINE Swap_cv(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_cv
#endif
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Swap two matrices.
!
! Description:
!   Exchanges the entries of the matrices `a` and `b`. It is available
!   for integer (`INT8`, `INT16`, `INT32`, `INT64`), real (`REAL32`,
!   `REAL64`) and complex (`DFPC`) matrices.
!
!   `a` and `b` should have the same shape. If they differ, only the
!   common block, `MIN(SIZE(a, k), SIZE(b, k))` for `k = 1, 2`, is
!   exchanged.
!
! Arguments:
!   a : INTEGER | REAL | COMPLEX, rank(2), INTENT(INOUT) - on return,
!     the old `b`
!   b : INTEGER | REAL | COMPLEX, rank(2), INTENT(INOUT) - on return,
!     the old `a`, normally `SHAPE(b) = SHAPE(a)`
!
! Returns:
!   The entries of `a` and `b` are exchanged in place.
!
! Formula:
!   $$(a_{ij}, b_{ij}) \leftarrow (b_{ij}, a_{ij})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DFPC
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia(2, 3), ib(2, 3), ia0(2, 3), ib0(2, 3)
! REAL(DFP) :: ra(3, 2), rb(3, 2), ra0(3, 2), rb0(3, 2)
! COMPLEX(DFPC) :: ca(2, 2), cb(2, 2), ca0(2, 2), cb0(2, 2)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! integer matrices
! ia0 = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], [2, 3])
! ib0 = -ia0
! ia = ia0
! ib = ib0
! CALL Swap(ia, ib)
! CALL OK(ALL(ia .EQ. ib0), "integer: a")
! CALL OK(ALL(ib .EQ. ia0), "integer: b")
!
! ! real matrices
! ra0 = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
!               [3, 2])
! rb0 = 0.5_DFP * ra0
! ra = ra0
! rb = rb0
! CALL Swap(ra, rb)
! CALL OK(ALL(ABS(ra - rb0) .LT. tol), "real: a")
! CALL OK(ALL(ABS(rb - ra0) .LT. tol), "real: b")
!
! ! complex matrices
! ca0 = RESHAPE([(1.0_DFP, 0.0_DFP), (0.0_DFP, 1.0_DFP), &
!                (2.0_DFP, 2.0_DFP), (3.0_DFP, -1.0_DFP)], [2, 2])
! cb0 = CONJG(ca0)
! ca = ca0
! cb = cb0
! CALL Swap(ca, cb)
! CALL OK(ALL(ABS(ca - cb0) .LT. tol), "complex: a")
! CALL OK(ALL(ABS(cb - ca0) .LT. tol), "complex: b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8m(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int8m

  MODULE PURE SUBROUTINE Swap_Int16m(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int16m

  MODULE PURE SUBROUTINE Swap_Int32m(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int32m

  MODULE PURE SUBROUTINE Swap_Int64m(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int64m

#ifdef USE_Int128
  MODULE PURE SUBROUTINE Swap_Int128m(a, b)
    INTEGER(Int128), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int128m
#endif

  MODULE PURE SUBROUTINE Swap_r32m(a, b)
    REAL(REAL32), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_r32m

  MODULE PURE SUBROUTINE Swap_r64m(a, b)
    REAL(REAL64), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_r64m

  MODULE PURE SUBROUTINE Swap_cm(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_cm
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Swap two scalars if a mask is true.
!
! Description:
!   Exchanges the values of the scalars `a` and `b` if `mask` is true.
!   If `mask` is false, `a` and `b` are not changed. It is available
!   for integer (`INT8`, `INT16`, `INT32`, `INT64`) and real (`REAL32`,
!   `REAL64`) scalars. There is no complex version.
!
!   A common use is `CALL Swap(a, b, a .LT. b)`, which puts the larger
!   value in `a`.
!
! Arguments:
!   a : INTEGER | REAL, INTENT(INOUT) - first value
!   b : INTEGER | REAL, INTENT(INOUT) - second value
!   mask : LOGICAL(LGT), INTENT(IN) - swap only if true
!
! Returns:
!   `a` and `b` are exchanged in place if `mask` is true.
!
! Formula:
!   $$(a, b) \leftarrow \begin{cases}
!     (b, a), & \text{if mask} \\
!     (a, b), & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia, ib
! REAL(DFP) :: ra, rb
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! mask is true: the values are swapped
! ia = 3_I4B
! ib = 4_I4B
! CALL Swap(ia, ib, .TRUE._LGT)
! CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "integer, mask true")
!
! ! mask is false: nothing changes
! CALL Swap(ia, ib, .FALSE._LGT)
! CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "integer, mask false")
!
! ! a typical use: make sure that a >= b
! ra = 3.0_DFP
! rb = 4.0_DFP
! CALL Swap(ra, rb, ra .LE. rb)
! CALL OK(ABS(ra - 4.0_DFP) .LT. tol .AND. ABS(rb - 3.0_DFP) .LT. tol, &
!         "real, a <= b")
!
! ! the second call does nothing because now a > b
! CALL Swap(ra, rb, ra .LE. rb)
! CALL OK(ABS(ra - 4.0_DFP) .LT. tol .AND. ABS(rb - 3.0_DFP) .LT. tol, &
!         "real, a > b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8s(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int8s

  MODULE PURE SUBROUTINE masked_Swap_Int16s(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int16s

  MODULE PURE SUBROUTINE masked_Swap_Int32s(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int32s

  MODULE PURE SUBROUTINE masked_Swap_Int64s(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int64s

#ifdef USE_Int128
  MODULE PURE SUBROUTINE masked_Swap_Int128s(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int128s
#endif

  MODULE PURE SUBROUTINE masked_Swap_r32s(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_r32s

  MODULE PURE SUBROUTINE masked_Swap_r64s(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_r64s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Swap the entries of two vectors where a mask is true.
!
! Description:
!   Exchanges `a(i)` and `b(i)` for every `i` where `mask(i)` is true.
!   The other entries are not changed. It is available for integer
!   (`INT8`, `INT16`, `INT32`, `INT64`) and real (`REAL32`, `REAL64`)
!   vectors. There is no complex version.
!
!   `a`, `b` and `mask` should have the same size. This is not
!   checked.
!
! Arguments:
!   a : INTEGER | REAL, rank(1), INTENT(INOUT) - first vector
!   b : INTEGER | REAL, rank(1), INTENT(INOUT) - second vector,
!     `SIZE(b) = SIZE(a)`
!   mask : LOGICAL(LGT), rank(1), INTENT(IN) - entries to swap,
!     `SIZE(mask) = SIZE(a)`
!
! Returns:
!   `a(i)` and `b(i)` are exchanged in place where `mask(i)` is true.
!
! Formula:
!   $$(a_i, b_i) \leftarrow (b_i, a_i) \quad \text{if } m_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia(4), ib(4)
! REAL(DFP) :: ra(3), rb(3)
! LOGICAL(LGT) :: mask(4)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! only the entries where mask is true are swapped
! ia = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
! ib = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
! mask = [.TRUE., .FALSE., .TRUE., .FALSE.]
! CALL Swap(ia, ib, mask)
! CALL OK(ALL(ia .EQ. [10_I4B, 2_I4B, 30_I4B, 4_I4B]), "integer: a")
! CALL OK(ALL(ib .EQ. [1_I4B, 20_I4B, 3_I4B, 40_I4B]), "integer: b")
!
! ! all false: nothing changes
! mask = .FALSE.
! CALL Swap(ia, ib, mask)
! CALL OK(ALL(ia .EQ. [10_I4B, 2_I4B, 30_I4B, 4_I4B]), "all false")
!
! ! a typical use: put the larger entry in a
! ra = [1.0_DFP, 5.0_DFP, 2.0_DFP]
! rb = [3.0_DFP, 4.0_DFP, 2.0_DFP]
! CALL Swap(ra, rb, ra .LT. rb)
! CALL OK(ALL(ABS(ra - [3.0_DFP, 5.0_DFP, 2.0_DFP]) .LT. tol), "real: a")
! CALL OK(ALL(ABS(rb - [1.0_DFP, 4.0_DFP, 2.0_DFP]) .LT. tol), "real: b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8v(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int8v

  MODULE PURE SUBROUTINE masked_Swap_Int16v(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int16v

  MODULE PURE SUBROUTINE masked_Swap_Int32v(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int32v

  MODULE PURE SUBROUTINE masked_Swap_Int64v(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int64v

#ifdef USE_Int128
  MODULE PURE SUBROUTINE masked_Swap_Int128v(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int128v
#endif

  MODULE PURE SUBROUTINE masked_Swap_r32v(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_r32v

  MODULE PURE SUBROUTINE masked_Swap_r64v(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_r64v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Swap the entries of two matrices where a mask is true.
!
! Description:
!   Exchanges `a(i, j)` and `b(i, j)` for every `(i, j)` where
!   `mask(i, j)` is true. The other entries are not changed. It is
!   available for integer (`INT8`, `INT16`, `INT32`, `INT64`) and real
!   (`REAL32`, `REAL64`) matrices. There is no complex version.
!
!   `a`, `b` and `mask` should have the same shape. This is not
!   checked.
!
! Arguments:
!   a : INTEGER | REAL, rank(2), INTENT(INOUT) - first matrix
!   b : INTEGER | REAL, rank(2), INTENT(INOUT) - second matrix,
!     `SHAPE(b) = SHAPE(a)`
!   mask : LOGICAL(LGT), rank(2), INTENT(IN) - entries to swap,
!     `SHAPE(mask) = SHAPE(a)`
!
! Returns:
!   `a(i, j)` and `b(i, j)` are exchanged in place where `mask(i, j)`
!   is true.
!
! Formula:
!   $$(a_{ij}, b_{ij}) \leftarrow (b_{ij}, a_{ij}) \quad
!     \text{if } m_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ia(2, 2), ib(2, 2)
! REAL(DFP) :: ra(2, 2), rb(2, 2), ra0(2, 2), rb0(2, 2)
! LOGICAL(LGT) :: mask(2, 2)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! swap only the diagonal
! ia = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B], [2, 2])
! ib = RESHAPE([10_I4B, 20_I4B, 30_I4B, 40_I4B], [2, 2])
! mask = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])
! CALL Swap(ia, ib, mask)
! CALL OK(ALL(ia .EQ. RESHAPE([10_I4B, 2_I4B, 3_I4B, 40_I4B], [2, 2])), &
!         "integer: a")
! CALL OK(ALL(ib .EQ. RESHAPE([1_I4B, 20_I4B, 30_I4B, 4_I4B], [2, 2])), &
!         "integer: b")
!
! ! put the larger entry in a
! ra0 = RESHAPE([1.0_DFP, 6.0_DFP, 2.0_DFP, 8.0_DFP], [2, 2])
! rb0 = RESHAPE([5.0_DFP, 3.0_DFP, 7.0_DFP, 4.0_DFP], [2, 2])
! ra = ra0
! rb = rb0
! CALL Swap(ra, rb, ra .LT. rb)
! CALL OK(ALL(ABS(ra - MAX(ra0, rb0)) .LT. tol), "real: a = max")
! CALL OK(ALL(ABS(rb - MIN(ra0, rb0)) .LT. tol), "real: b = min")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8m(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int8m

  MODULE PURE SUBROUTINE masked_Swap_Int16m(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int16m

  MODULE PURE SUBROUTINE masked_Swap_Int32m(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int32m

  MODULE PURE SUBROUTINE masked_Swap_Int64m(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int64m

#ifdef USE_Int128
  MODULE PURE SUBROUTINE masked_Swap_Int128m(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int128m
#endif

  MODULE PURE SUBROUTINE masked_Swap_r32m(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_r32m

  MODULE PURE SUBROUTINE masked_Swap_r64m(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_r64m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-2 real array.
!
! Description:
!   Returns in `a` the array `b` with its two indices permuted.
!   Dimension 1 of `a` runs along dimension `i1` of `b`, and
!   dimension 2 of `a` runs along dimension `i2` of `b`.
!
!   - `(i1, i2) = (1, 2)` gives `a = b`.
!   - `(i1, i2) = (2, 1)` gives `a = TRANSPOSE(b)`.
!   - If `i1` or `i2` is not 1 or 2, or if `i1 = i2`, then `a` is a
!     copy of `b`.
!
!   `a` is (re)allocated to the shape `(s(i1), s(i2))`, where
!   `s = SHAPE(b)`. `b` is not changed. It is available for `REAL32`
!   and `REAL64`. Use `Swap_` if `a` is already allocated.
!
! Arguments:
!   a : REAL, rank(2), ALLOCATABLE, INTENT(INOUT) - the result
!   b : REAL, rank(2), INTENT(IN) - the input array
!   i1 : INTEGER(I4B), INTENT(IN) - dimension of `b` for dimension 1
!     of `a`, `1 <= i1 <= 2`
!   i2 : INTEGER(I4B), INTENT(IN) - dimension of `b` for dimension 2
!     of `a`, `1 <= i2 <= 2`
!
! Returns:
!   a : REAL, rank(2) - `SHAPE(a) = (s(i1), s(i2))`
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}) = b(p_1, p_2), \quad
!     \mathbf{p} = (i, j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(3, 4)
! REAL(DFP), ALLOCATABLE :: a(:, :)
! INTEGER(I4B) :: i, j
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j) = 10*i + j
! DO j = 1, 4
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! i1 = 1, i2 = 2: a is a copy of b
! CALL Swap(a=a, b=b, i1=1, i2=2)
! CALL OK(ALL(SHAPE(a) .EQ. [3, 4]), "identity: shape")
! CALL OK(ALL(ABS(a - b) .LT. tol), "identity: a = b")
!
! ! i1 = 2, i2 = 1: a is the transpose of b
! CALL Swap(a=a, b=b, i1=2, i2=1)
! CALL OK(ALL(SHAPE(a) .EQ. [4, 3]), "transpose: shape")
! CALL OK(ALL(ABS(a - TRANSPOSE(b)) .LT. tol), "transpose: a = b^T")
! CALL OK(ABS(a(4, 3) - 34.0_DFP) .LT. tol, "transpose: a(4,3)")
!
! ! i1 = i2 is not a permutation: a is a copy of b
! CALL Swap(a=a, b=b, i1=1, i2=1)
! CALL OK(ALL(SHAPE(a) .EQ. [3, 4]), "i1 = i2: shape")
! CALL OK(ALL(ABS(a - b) .LT. tol), "i1 = i2: a = b")
!
! ! an index out of range: a is a copy of b
! CALL Swap(a=a, b=b, i1=3, i2=1)
! CALL OK(ALL(ABS(a - b) .LT. tol), "out of range: a = b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index1(a, b, i1, i2)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index1

  MODULE PURE SUBROUTINE Swap_index2(a, b, i1, i2)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index2
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                              Swap_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-2 real array without allocation.
!
! Description:
!   Same as `Swap(a, b, i1, i2)`, but `a` is not allocated. Only the
!   leading block `a(1:s(i1), 1:s(i2))` is written, where
!   `s = SHAPE(b)`; the other entries of `a` are not changed.
!
!   - `a` must be large enough: `SIZE(a, 1) >= s(i1)` and
!     `SIZE(a, 2) >= s(i2)`.
!   - `(i1, i2)` must be `(1, 2)` or `(2, 1)`. Nothing is checked,
!     so other values give a wrong result or an out-of-bounds write.
!
!   It is available for `REAL32` and `REAL64`.
!
! Arguments:
!   a : REAL, rank(2), INTENT(INOUT) - the result
!   b : REAL, rank(2), INTENT(IN) - the input array
!   i1 : INTEGER(I4B), INTENT(IN) - dimension of `b` for dimension 1
!     of `a`
!   i2 : INTEGER(I4B), INTENT(IN) - dimension of `b` for dimension 2
!     of `a`
!
! Returns:
!   a : REAL, rank(2) - `a(1:s(i1), 1:s(i2))` holds the result
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}) = b(p_1, p_2), \quad
!     \mathbf{p} = (i, j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE SwapUtility, ONLY: Swap_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(3, 4), a(5, 5)
! INTEGER(I4B) :: i, j
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j) = 10*i + j
! DO j = 1, 4
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! i1 = 2, i2 = 1: a(1:4, 1:3) is the transpose of b
! a = -1.0_DFP
! CALL Swap_(a=a, b=b, i1=2, i2=1)
! CALL OK(ALL(ABS(a(1:4, 1:3) - TRANSPOSE(b)) .LT. tol), &
!         "transpose: leading block")
!
! ! entries outside the leading block are not changed
! CALL OK(ALL(ABS(a(5, :) + 1.0_DFP) .LT. tol), "row 5 untouched")
! CALL OK(ALL(ABS(a(:, 4:5) + 1.0_DFP) .LT. tol), "columns 4:5 untouched")
!
! ! i1 = 1, i2 = 2: a(1:3, 1:4) is a copy of b
! a = -1.0_DFP
! CALL Swap_(a=a, b=b, i1=1, i2=2)
! CALL OK(ALL(ABS(a(1:3, 1:4) - b) .LT. tol), "identity: leading block")
! CALL OK(ALL(ABS(a(4:5, :) + 1.0_DFP) .LT. tol), "rows 4:5 untouched")
!
! END PROGRAM main
!```

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_1(a, b, i1, i2)
    REAL(REAL32), INTENT(INOUT) :: a(:, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index_1

  MODULE PURE SUBROUTINE Swap_index_2(a, b, i1, i2)
    REAL(REAL64), INTENT(INOUT) :: a(:, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index_2
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-3 real array.
!
! Description:
!   Returns in `a` the array `b` with its three indices permuted.
!   Dimension `k` of `a` runs along dimension `ik` of `b`. For
!   example, `(i1, i2, i3) = (1, 3, 2)` gives
!   `a(i, k, j) = b(i, j, k)`.
!
!   - `a` is (re)allocated to the shape `(s(i1), s(i2), s(i3))`,
!     where `s = SHAPE(b)`. `b` is not changed.
!   - If any of `i1`, `i2`, `i3` is not in `1..3`, then `a` is a copy
!     of `b`.
!   - Repeated indices are not checked. They give a result with the
!     wrong values, so `(i1, i2, i3)` should be a permutation of
!     `(1, 2, 3)`. For example, `(1, 1, 2)` gives `a` the shape
!     `(s(1), s(1), s(2))` and writes `a(i, i, j) = b(i, j, k)`:
!     every `k` writes the same entry, and entries with unequal
!     first two indices are not set.
!
!   It is available for `REAL32` and `REAL64`. Use `Swap_` if `a` is
!   already allocated.
!
! Arguments:
!   a : REAL, rank(3), ALLOCATABLE, INTENT(INOUT) - the result
!   b : REAL, rank(3), INTENT(IN) - the input array
!   i1, i2, i3 : INTEGER(I4B), INTENT(IN) - dimensions of `b` for
!     dimensions 1, 2 and 3 of `a`
!
! Returns:
!   a : REAL, rank(3) - `SHAPE(a) = (s(i1), s(i2), s(i3))`
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}, p_{i_3}) = b(p_1, p_2, p_3), \quad
!     \mathbf{p} = (i, j, k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(2, 3, 4)
! REAL(DFP), ALLOCATABLE :: a(:, :, :)
! INTEGER(I4B) :: i, j, k
! LOGICAL(LGT) :: isok
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j, k) = 100*i + 10*j + k
! DO k = 1, 4
!   DO j = 1, 3
!     DO i = 1, 2
!       b(i, j, k) = REAL(100 * i + 10 * j + k, DFP)
!     END DO
!   END DO
! END DO
!
! ! (1, 2, 3): a is a copy of b
! CALL Swap(a, b, 1, 2, 3)
! CALL OK(ALL(SHAPE(a) .EQ. [2, 3, 4]), "(1,2,3): shape")
! CALL OK(ALL(ABS(a - b) .LT. tol), "(1,2,3): a = b")
!
! ! (1, 3, 2): a(i, k, j) = b(i, j, k)
! CALL Swap(a, b, 1, 3, 2)
! CALL OK(ALL(SHAPE(a) .EQ. [2, 4, 3]), "(1,3,2): shape")
! isok = .TRUE.
! DO k = 1, 4
!   DO j = 1, 3
!     DO i = 1, 2
!       isok = isok .AND. ABS(a(i, k, j) - b(i, j, k)) .LT. tol
!     END DO
!   END DO
! END DO
! CALL OK(isok, "(1,3,2): a(i,k,j) = b(i,j,k)")
! CALL OK(ABS(a(2, 4, 1) - 214.0_DFP) .LT. tol, "(1,3,2): a(2,4,1)")
!
! ! (2, 3, 1): a(j, k, i) = b(i, j, k)
! CALL Swap(a, b, 2, 3, 1)
! CALL OK(ALL(SHAPE(a) .EQ. [3, 4, 2]), "(2,3,1): shape")
! CALL OK(ABS(a(3, 4, 2) - 234.0_DFP) .LT. tol, "(2,3,1): a(3,4,2)")
! CALL OK(ABS(a(1, 2, 2) - 212.0_DFP) .LT. tol, "(2,3,1): a(1,2,2)")
!
! ! (3, 1, 2): a(k, i, j) = b(i, j, k)
! CALL Swap(a, b, 3, 1, 2)
! CALL OK(ALL(SHAPE(a) .EQ. [4, 2, 3]), "(3,1,2): shape")
! CALL OK(ABS(a(4, 1, 2) - 124.0_DFP) .LT. tol, "(3,1,2): a(4,1,2)")
!
! ! an index out of range: a is a copy of b
! CALL Swap(a, b, 4, 1, 2)
! CALL OK(ALL(ABS(a - b) .LT. tol), "out of range: a = b")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index3(a, b, i1, i2, i3)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index3

  MODULE PURE SUBROUTINE Swap_index4(a, b, i1, i2, i3)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index4
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                              Swap_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-3 real array without allocation.
!
! Description:
!   Same as `Swap(a, b, i1, i2, i3)`, but `a` is not allocated. Only
!   the leading block `a(1:s(i1), 1:s(i2), 1:s(i3))` is written,
!   where `s = SHAPE(b)`; the other entries of `a` are not changed.
!
!   - `a` must be large enough: `SIZE(a, k) >= s(ik)`.
!   - `(i1, i2, i3)` must be a permutation of `(1, 2, 3)`. Nothing is
!     checked, so other values give a wrong result or an
!     out-of-bounds access.
!
!   It is available for `REAL32` and `REAL64`.
!
! Arguments:
!   a : REAL, rank(3), INTENT(INOUT) - the result
!   b : REAL, rank(3), INTENT(IN) - the input array
!   i1, i2, i3 : INTEGER(I4B), INTENT(IN) - dimensions of `b` for
!     dimensions 1, 2 and 3 of `a`
!
! Returns:
!   a : REAL, rank(3) - `a(1:s(i1), 1:s(i2), 1:s(i3))` holds the
!     result
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}, p_{i_3}) = b(p_1, p_2, p_3), \quad
!     \mathbf{p} = (i, j, k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE SwapUtility, ONLY: Swap
! USE SwapUtility, ONLY: Swap_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(2, 3, 4), a(3, 4, 2)
! REAL(DFP), ALLOCATABLE :: c(:, :, :)
! INTEGER(I4B) :: i, j, k
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j, k) = 100*i + 10*j + k
! DO k = 1, 4
!   DO j = 1, 3
!     DO i = 1, 2
!       b(i, j, k) = REAL(100 * i + 10 * j + k, DFP)
!     END DO
!   END DO
! END DO
!
! ! (2, 3, 1): a(j, k, i) = b(i, j, k), a has the right shape
! CALL Swap_(a, b, 2, 3, 1)
! CALL OK(ABS(a(3, 4, 2) - 234.0_DFP) .LT. tol, "a(3,4,2)")
! CALL OK(ABS(a(1, 2, 2) - 212.0_DFP) .LT. tol, "a(1,2,2)")
!
! ! Swap_ gives the same result as Swap
! CALL Swap(c, b, 2, 3, 1)
! CALL OK(ALL(ABS(a - c) .LT. tol), "same as Swap")
!
! END PROGRAM main
!```

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_3(a, b, i1, i2, i3)
    REAL(REAL32), INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index_3

  MODULE PURE SUBROUTINE Swap_index_4(a, b, i1, i2, i3)
    REAL(REAL64), INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index_4
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-4 real array.
!
! Description:
!   Returns in `a` the array `b` with its four indices permuted.
!   Dimension `k` of `a` runs along dimension `ik` of `b`. For
!   example, `(i1, i2, i3, i4) = (4, 3, 2, 1)` gives
!   `a(l, k, j, i) = b(i, j, k, l)`.
!
!   - `a` is (re)allocated to the shape
!     `(s(i1), s(i2), s(i3), s(i4))`, where `s = SHAPE(b)`. `b` is
!     not changed.
!   - Unlike rank 2 and 3, the indices are not checked at all, so
!     `(i1, i2, i3, i4)` must be a permutation of `(1, 2, 3, 4)`.
!     An index outside `1..4` gives an out-of-bounds access, and a
!     repeated index gives wrong values, as described for rank 3.
!
!   It is available for `REAL32` and `REAL64`. Use `Swap_` if `a` is
!   already allocated.
!
! Arguments:
!   a : REAL, rank(4), ALLOCATABLE, INTENT(INOUT) - the result
!   b : REAL, rank(4), INTENT(IN) - the input array
!   i1, i2, i3, i4 : INTEGER(I4B), INTENT(IN) - dimensions of `b` for
!     dimensions 1, 2, 3 and 4 of `a`
!
! Returns:
!   a : REAL, rank(4) - `SHAPE(a) = (s(i1), s(i2), s(i3), s(i4))`
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}, p_{i_3}, p_{i_4}) = b(p_1, p_2, p_3, p_4),
!     \quad \mathbf{p} = (i, j, k, l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE SwapUtility, ONLY: Swap
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(2, 3, 4, 5)
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
! INTEGER(I4B) :: i, j, k, l
! LOGICAL(LGT) :: isok
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j, k, l) = 1000*i + 100*j + 10*k + l
! DO l = 1, 5
!   DO k = 1, 4
!     DO j = 1, 3
!       DO i = 1, 2
!         b(i, j, k, l) = REAL(1000 * i + 100 * j + 10 * k + l, DFP)
!       END DO
!     END DO
!   END DO
! END DO
!
! ! (1, 2, 3, 4): a is a copy of b
! CALL Swap(a, b, 1, 2, 3, 4)
! CALL OK(ALL(SHAPE(a) .EQ. [2, 3, 4, 5]), "(1,2,3,4): shape")
! CALL OK(ALL(ABS(a - b) .LT. tol), "(1,2,3,4): a = b")
!
! ! (1, 3, 2, 4): a(i, k, j, l) = b(i, j, k, l)
! CALL Swap(a, b, 1, 3, 2, 4)
! CALL OK(ALL(SHAPE(a) .EQ. [2, 4, 3, 5]), "(1,3,2,4): shape")
! isok = .TRUE.
! DO l = 1, 5
!   DO k = 1, 4
!     DO j = 1, 3
!       DO i = 1, 2
!         isok = isok .AND. ABS(a(i, k, j, l) - b(i, j, k, l)) .LT. tol
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "(1,3,2,4): a(i,k,j,l) = b(i,j,k,l)")
!
! ! (4, 3, 2, 1): a(l, k, j, i) = b(i, j, k, l)
! CALL Swap(a, b, 4, 3, 2, 1)
! CALL OK(ALL(SHAPE(a) .EQ. [5, 4, 3, 2]), "(4,3,2,1): shape")
! CALL OK(ABS(a(5, 4, 3, 2) - 2345.0_DFP) .LT. tol, "(4,3,2,1): a(5,4,3,2)")
! CALL OK(ABS(a(1, 2, 3, 1) - 1321.0_DFP) .LT. tol, "(4,3,2,1): a(1,2,3,1)")
!
! END PROGRAM main
!```

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index5(a, b, i1, i2, i3, i4)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! dimension 4 of a runs along dimension i4 of b
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index5

  MODULE PURE SUBROUTINE Swap_index6(a, b, i1, i2, i3, i4)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! dimension 4 of a runs along dimension i4 of b
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index6
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                              Swap_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Permute the indices of a rank-4 real array without allocation.
!
! Description:
!   Same as `Swap(a, b, i1, i2, i3, i4)`, but `a` is not allocated.
!   Only the leading block `a(1:s(i1), ..., 1:s(i4))` is written,
!   where `s = SHAPE(b)`; the other entries of `a` are not changed.
!
!   - `a` must be large enough: `SIZE(a, k) >= s(ik)`.
!   - `(i1, i2, i3, i4)` must be a permutation of `(1, 2, 3, 4)`.
!     Nothing is checked, so other values give a wrong result or an
!     out-of-bounds access.
!
!   It is available for `REAL32` and `REAL64`.
!
! Arguments:
!   a : REAL, rank(4), INTENT(INOUT) - the result
!   b : REAL, rank(4), INTENT(IN) - the input array
!   i1, i2, i3, i4 : INTEGER(I4B), INTENT(IN) - dimensions of `b` for
!     dimensions 1, 2, 3 and 4 of `a`
!
! Returns:
!   a : REAL, rank(4) - `a(1:s(i1), ..., 1:s(i4))` holds the result
!
! Formula:
!   $$a(p_{i_1}, p_{i_2}, p_{i_3}, p_{i_4}) = b(p_1, p_2, p_3, p_4),
!     \quad \mathbf{p} = (i, j, k, l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE SwapUtility, ONLY: Swap
! USE SwapUtility, ONLY: Swap_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: b(2, 3, 4, 5), a(5, 4, 3, 2)
! REAL(DFP), ALLOCATABLE :: c(:, :, :, :)
! INTEGER(I4B) :: i, j, k, l
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! b(i, j, k, l) = 1000*i + 100*j + 10*k + l
! DO l = 1, 5
!   DO k = 1, 4
!     DO j = 1, 3
!       DO i = 1, 2
!         b(i, j, k, l) = REAL(1000 * i + 100 * j + 10 * k + l, DFP)
!       END DO
!     END DO
!   END DO
! END DO
!
! ! (4, 3, 2, 1): a(l, k, j, i) = b(i, j, k, l)
! ! a does not have to be allocatable
! CALL Swap_(a, b, 4, 3, 2, 1)
! CALL OK(ALL(SHAPE(a) .EQ. [5, 4, 3, 2]), "shape is not changed")
! CALL OK(ABS(a(5, 4, 3, 2) - 2345.0_DFP) .LT. tol, "a(5,4,3,2)")
! CALL OK(ABS(a(1, 2, 3, 1) - 1321.0_DFP) .LT. tol, "a(1,2,3,1)")
!
! ! Swap_ gives the same result as Swap
! CALL Swap(c, b, 4, 3, 2, 1)
! CALL OK(ALL(ABS(a - c) .LT. tol), "same as Swap")
!
! END PROGRAM main
!```

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_5(a, b, i1, i2, i3, i4)
    REAL(REAL32), INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! dimension 4 of a runs along dimension i4 of b
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index_5

  MODULE PURE SUBROUTINE Swap_index_6(a, b, i1, i2, i3, i4)
    REAL(REAL64), INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! dimension 1 of a runs along dimension i1 of b
    !! make sure i1 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! dimension 2 of a runs along dimension i2 of b
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! dimension 3 of a runs along dimension i3 of b
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! dimension 4 of a runs along dimension i4 of b
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index_6
END INTERFACE Swap_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SwapUtility
