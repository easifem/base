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
! date: 2022-12-12
! summary: Makes a symmetric matrix from one triangle of a matrix.
!
! Description:
!   This module contains two generic routines that build a symmetric
!   matrix from the upper or the lower triangle of a square dense
!   matrix.
!
!   - `Sym(mat, from)` is a function. It returns a new matrix and does
!     not change `mat`.
!   - `GetSym(mat, from)` is a subroutine. It changes `mat` in place
!     and does not allocate any extra memory, so prefer it for large
!     matrices.
!
!   `from = "U"` (or `"u"`) copies the upper triangle into the lower
!   one, and `from = "L"` (or `"l"`) copies the lower triangle into the
!   upper one. The diagonal is kept.
!
!   Both routines are available for integers (`INT8`, `INT16`, `INT32`,
!   `INT64`) and reals (`REAL32`, `REAL64`).

MODULE SymUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: Sym
PUBLIC :: GetSym

!----------------------------------------------------------------------------
!                                                                       Sym
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-12
! summary: Returns a symmetric matrix from the upper or lower triangle.
!
! Description:
!   Returns a symmetric matrix built from one triangle of the square
!   matrix `mat`. The input `mat` is not changed.
!
!   - `from = "U"` or `"u"`: the upper triangle (with the diagonal) of
!     `mat` is used, and the lower triangle of `ans` is its mirror.
!   - `from = "L"` or `"l"`: the lower triangle (with the diagonal) of
!     `mat` is used, and the upper triangle of `ans` is its mirror.
!   - Any other value of `from` leaves `ans` undefined.
!   - `mat` must be square. This is not checked; a rectangular `mat`
!     is accessed out of bounds.
!
!   For a large matrix use `GetSym`, which works in place.
!
! Arguments:
!   mat : INTEGER or REAL, rank(2) - square matrix,
!     `SIZE(mat, 1) = SIZE(mat, 2)`
!   from : CHARACTER(1) - `"U"`/`"u"` or `"L"`/`"l"`
!
! Returns:
!   ans : same type and kind as `mat`, rank(2) - symmetric matrix,
!     `SHAPE(ans) = SHAPE(mat)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \begin{cases}
!     \mathrm{mat}_{\min(i,j),\max(i,j)}, & \text{from = U} \\
!     \mathrm{mat}_{\max(i,j),\min(i,j)}, & \text{from = L}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SymUtility, ONLY: Sym
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: mat(3, 3), ans(3, 3), expected(3, 3), one(1, 1)
!
! ! mat = | 1 4 7 |
! !       | 2 5 8 |
! !       | 3 6 9 |
! mat = RESHAPE([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
!
! ! use the upper triangle
! ans = Sym(mat, "U")
! expected = RESHAPE([1, 4, 7, 4, 5, 8, 7, 8, 9], [3, 3])
! CALL OK(ALL(ans .EQ. expected), "from upper triangle")
! CALL OK(ALL(ans .EQ. TRANSPOSE(ans)), "result is symmetric")
!
! ! use the lower triangle
! ans = Sym(mat, "L")
! expected = RESHAPE([1, 2, 3, 2, 5, 6, 3, 6, 9], [3, 3])
! CALL OK(ALL(ans .EQ. expected), "from lower triangle")
! CALL OK(ALL(ans .EQ. TRANSPOSE(ans)), "result is symmetric")
!
! ! lower case is also accepted
! CALL OK(ALL(Sym(mat, "u") .EQ. Sym(mat, "U")), "from = u")
! CALL OK(ALL(Sym(mat, "l") .EQ. Sym(mat, "L")), "from = l")
!
! ! the input is not changed
! CALL OK(mat(2, 1) .EQ. 2_I4B .AND. mat(1, 2) .EQ. 4_I4B, &
!         "mat is unchanged")
!
! ! 1x1 matrix
! one = 5_I4B
! CALL OK(ALL(Sym(one, "U") .EQ. 5_I4B), "1x1 matrix")
!
! END PROGRAM main
!```

INTERFACE Sym
  MODULE PURE FUNCTION Sym_Int8(mat, from) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(INT8) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int8

  MODULE PURE FUNCTION Sym_Int16(mat, from) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(INT16) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int16

  MODULE PURE FUNCTION Sym_Int32(mat, from) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(INT32) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int32

  MODULE PURE FUNCTION Sym_Int64(mat, from) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(INT64) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int64

  MODULE PURE FUNCTION Sym_Real32(mat, from) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    REAL(REAL32) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Real32

  MODULE PURE FUNCTION Sym_Real64(mat, from) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    REAL(REAL64) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Real64
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                                    GetSym
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-12
! summary: Makes a matrix symmetric in place from one of its triangles.
!
! Description:
!   Overwrites one triangle of the square matrix `mat` with the mirror
!   of the other one, so that `mat` becomes symmetric. It gives the
!   same result as `mat = Sym(mat, from)` but needs no extra memory.
!
!   - `from = "U"` or `"u"`: the upper triangle is kept and copied into
!     the lower triangle.
!   - `from = "L"` or `"l"`: the lower triangle is kept and copied into
!     the upper triangle.
!   - Any other value of `from` leaves `mat` unchanged.
!   - `mat` must be square. This is not checked; a rectangular `mat`
!     is accessed out of bounds.
!
! Arguments:
!   mat : INTEGER or REAL, rank(2), INTENT(INOUT) - square matrix,
!     `SIZE(mat, 1) = SIZE(mat, 2)`; symmetric on return
!   from : CHARACTER(1) - `"U"`/`"u"` or `"L"`/`"l"`
!
! Returns:
!   mat is changed in place.
!
! Formula:
!   $$\mathrm{mat}_{ij} \leftarrow \begin{cases}
!     \mathrm{mat}_{ji}, & i > j, \ \text{from = U} \\
!     \mathrm{mat}_{ji}, & i < j, \ \text{from = L}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SymUtility, ONLY: GetSym
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: mat(3, 3), mat0(3, 3), expected(3, 3)
!
! ! mat0 = | 1 4 7 |
! !        | 2 5 8 |
! !        | 3 6 9 |
! mat0 = RESHAPE([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
!
! ! use the upper triangle, mat is changed in place
! mat = mat0
! CALL GetSym(mat, "U")
! expected = RESHAPE([1, 4, 7, 4, 5, 8, 7, 8, 9], [3, 3])
! CALL OK(ALL(mat .EQ. expected), "from upper triangle")
! CALL OK(ALL(mat .EQ. TRANSPOSE(mat)), "result is symmetric")
!
! ! use the lower triangle
! mat = mat0
! CALL GetSym(mat, "L")
! expected = RESHAPE([1, 2, 3, 2, 5, 6, 3, 6, 9], [3, 3])
! CALL OK(ALL(mat .EQ. expected), "from lower triangle")
!
! ! lower case is also accepted
! mat = mat0
! CALL GetSym(mat, "l")
! CALL OK(ALL(mat .EQ. expected), "from = l")
!
! ! any other value leaves mat unchanged
! mat = mat0
! CALL GetSym(mat, "X")
! CALL OK(ALL(mat .EQ. mat0), "unknown from does nothing")
!
! END PROGRAM main
!```

INTERFACE GetSym
  MODULE PURE SUBROUTINE GetSym_Int8(mat, from)
    INTEGER(INT8), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int8

  MODULE PURE SUBROUTINE GetSym_Int16(mat, from)
    INTEGER(INT16), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int16

  MODULE PURE SUBROUTINE GetSym_Int32(mat, from)
    INTEGER(INT32), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int32

  MODULE PURE SUBROUTINE GetSym_Int64(mat, from)
    INTEGER(INT64), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int64

  MODULE PURE SUBROUTINE GetSym_Real32(mat, from)
    REAL(REAL32), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Real32

  MODULE PURE SUBROUTINE GetSym_Real64(mat, from)
    REAL(REAL64), INTENT(INOUT) :: mat(:, :)
    CHARACTER(1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Real64
END INTERFACE GetSym

END MODULE SymUtility
