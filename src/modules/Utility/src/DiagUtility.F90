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
! date: 2022-12-11
! summary: Routines related to diagonal and tridiagonal dense matrices.
!
! Description:
!   This module contains useful routines for diagonal matrices.
!
!   - `Diag` makes a diagonal matrix from a vector, or extracts a
!     diagonal from a square matrix.
!   - `SetDiag` sets a diagonal of a square matrix.
!   - `DiagSize` returns the number of entries on a diagonal of a square
!     or rectangle matrix.
!   - `DiagIndx` returns the (row, column) indices of the entries on a
!     diagonal of a square or rectangle matrix.
!   - `TriDiag` makes a tridiagonal matrix from three vectors.
!   - `SetTriDiag` sets the three main diagonals of a square matrix.
!
! Diagonal numbering:
!   A diagonal is identified by an integer `diagNo`, which is the
!   column index minus the row index of its entries.
!
!   - `diagNo = 0` is the main diagonal,
!   - `diagNo > 0` is a super-diagonal (above the main diagonal),
!   - `diagNo < 0` is a sub-diagonal (below the main diagonal).
!
!   $$\mathrm{diagNo} = j - i \quad \mathrm{for\ the\ entry}\ A_{ij}$$
!
! Supported types:
!   The vector arguments can be of kind `Int8`, `Int16`, `Int32`,
!   `Int64`, `Real32` or `Real64` (and `Int128` when `USE_Int128` is
!   defined). The matrices are always `REAL(DFP)`.

MODULE DiagUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64

#ifdef USE_Int128
USE GlobalData, ONLY: Int128
#endif

IMPLICIT NONE
PRIVATE

PUBLIC :: Diag
PUBLIC :: SetDiag
PUBLIC :: DiagSize
PUBLIC :: DiagIndx
PUBLIC :: TriDiag
PUBLIC :: SetTriDiag

!----------------------------------------------------------------------------
!                                                                       Diag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Make a diagonal matrix from a vector.
!
! Description:
!   Creates a square diagonal matrix whose main diagonal is the vector
!   `a`. All off-diagonal entries are zero.
!
!   - `Diag_1` ... `Diag_4` accept `Int8`, `Int16`, `Int32`, `Int64`.
!   - `Diag_5`, `Diag_6` accept `Real32`, `Real64`.
!   - The result is always an `n x n` matrix of `REAL(DFP)`, where
!     `n = SIZE(a)`.
!
! Arguments:
!   a : vector of size n, main diagonal of the result
!
! Returns:
!   ans : REAL(DFP), shape (SIZE(a), SIZE(a)) - diagonal matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = a_i\,\delta_{ij}, \qquad 1 \le i,j \le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: REAL64
! USE DiagUtility, ONLY: Diag
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(REAL64) :: d(n)
! REAL(DFP) :: mat(n, n), expected(n, n)
! INTEGER(I4B) :: ii
!
! d = [8.25_REAL64, 3.5_REAL64, 0.5_REAL64, 0.25_REAL64, 2.75_REAL64]
!
! expected = 0.0_DFP
! DO ii = 1, n
!   expected(ii, ii) = REAL(d(ii), DFP)
! END DO
!
! mat = Diag(d)
!
! CALL OK(ALL(SHAPE(mat) .EQ. [n, n]), "Diag(REAL64) shape")
! CALL OK(ALL(mat .EQ. expected), "Diag(REAL64) values")
!
! ! off-diagonal entries are exactly zero: clear the diagonal, rest is zero
! DO ii = 1, n
!   mat(ii, ii) = 0.0_DFP
! END DO
! CALL OK(ALL(mat .EQ. 0.0_DFP), "Diag(REAL64) off-diagonal is zero")
!
! END PROGRAM main
!```

INTERFACE Diag
  MODULE PURE FUNCTION Diag_1(a) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_1

  MODULE PURE FUNCTION Diag_2(a) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_2

  MODULE PURE FUNCTION Diag_3(a) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_3

  MODULE PURE FUNCTION Diag_4(a) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_4

  MODULE PURE FUNCTION Diag_5(a) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_5

  MODULE PURE FUNCTION Diag_6(a) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_6
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                                      Diag
!----------------------------------------------------------------------------

#ifdef USE_Int128

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Make diagonal matrix from Int128 vector.
!
! Description:
!   Same as `Diag_1` ... `Diag_6`, but for a vector of kind `Int128`.
!   This routine is available only when `USE_Int128` is defined.
!
! Arguments:
!   a : INTEGER(Int128), vector of size n
!
! Returns:
!   ans : REAL(DFP), shape (SIZE(a), SIZE(a)) - diagonal matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = a_i\,\delta_{ij}$$

INTERFACE Diag
  MODULE PURE FUNCTION Diag_7(a) RESULT(Ans)
    INTEGER(Int128), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_7
END INTERFACE Diag

#endif

!----------------------------------------------------------------------------
!                                                                       Diag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Get the diagNo-th diagonal of a square matrix.
!
! Description:
!   Returns the diagonal of the square matrix `mat` selected by
!   `diagNo`.
!
!   - `diagNo = 0` denotes the main diagonal.
!   - `diagNo > 0` denotes the super-diagonal.
!   - `diagNo < 0` denotes the sub-diagonal.
!
!   The result `ans` is a vector of default reals (DFP) and is
!   allocated by the function. Its size is `n - ABS(diagNo)`, where
!   `n = SIZE(mat, 1)`.
!
!   This function is meant for square matrices, and `ABS(diagNo)` should
!   be smaller than `n`.
!
! Arguments:
!   mat : REAL(DFP), rank 2 - square matrix of shape (n, n)
!   diagNo : INTEGER(I4B) - diagonal number
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, size n - ABS(diagNo)
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!   A_{i,\,i+k} & k \ge 0, \quad 1 \le i \le n-k \\
!   A_{i-k,\,i} & k < 0, \quad 1 \le i \le n+k
!   \end{cases}$$
!
!   where $k$ = `diagNo`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: Diag
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP) :: mat(n, n)
! INTEGER(I4B) :: ii, jj
!
! DO jj = 1, n
!   DO ii = 1, n
!     mat(ii, jj) = REAL(10 * ii + jj, DFP)
!   END DO
! END DO
!
! CALL OK(isEqual(Diag(mat, 0), REAL([11, 22, 33, 44, 55], DFP)), &
!         "Diag main diagonal")
! CALL OK(isEqual(Diag(mat, 1), REAL([12, 23, 34, 45], DFP)), &
!         "Diag first super-diagonal")
! CALL OK(isEqual(Diag(mat, 2), REAL([13, 24, 35], DFP)), &
!         "Diag second super-diagonal")
! CALL OK(isEqual(Diag(mat, 4), REAL([15], DFP)), &
!         "Diag last super-diagonal")
! CALL OK(isEqual(Diag(mat, -1), REAL([21, 32, 43, 54], DFP)), &
!         "Diag first sub-diagonal")
! CALL OK(isEqual(Diag(mat, -2), REAL([31, 42, 53], DFP)), &
!         "Diag second sub-diagonal")
! CALL OK(isEqual(Diag(mat, -4), REAL([51], DFP)), &
!         "Diag last sub-diagonal")
!
! CONTAINS
!
! PURE FUNCTION isEqual(a, b) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: a(:), b(:)
!   LOGICAL :: ans
!   ans = SIZE(a) .EQ. SIZE(b)
!   IF (ans) ans = ALL(ABS(a - b) .LT. tol)
! END FUNCTION isEqual
!
! END PROGRAM main
!```

INTERFACE Diag
  MODULE PURE FUNCTION Diag_8(mat, diagNo) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
    !! diagonal  number
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! diagonal
  END FUNCTION Diag_8
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                                   SetDiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Set the diagNo-th diagonal of a square matrix.
!
! Description:
!   Sets the diagonal of the square matrix `mat` selected by `diagNo`.
!   The entries of `mat` that are not on this diagonal are not modified.
!
!   - `diagNo = 0` denotes the main diagonal.
!   - `diagNo > 0` denotes the super-diagonal.
!   - `diagNo < 0` denotes the sub-diagonal.
!
!   The vector `d` can be of kind `Int8`, `Int16`, `Int32`, `Int64`,
!   `Real32` or `Real64`.
!
!   - If `SIZE(d) = 1`, then all entries of the diagonal are set to
!     this value (constant diagonal).
!   - If `SIZE(d) .NE. 1`, then the size of `d` should be at least the
!     size of the diagonal, which is `n - ABS(diagNo)`. For example, the
!     first sub-diagonal has `n - 1` entries.
!
!   Sizes are not checked at run time.
!
! Arguments:
!   mat : REAL(DFP), rank 2, INOUT - square matrix of shape (n, n)
!   d : vector of size 1 or at least n - ABS(diagNo)
!   diagNo : INTEGER(I4B) - diagonal number
!
! Formula:
!   $$A_{i,\,i+k} \leftarrow d_i \quad (k \ge 0), \qquad
!   A_{i-k,\,i} \leftarrow d_i \quad (k < 0)$$
!
!   for $1 \le i \le n - |k|$, where $k$ = `diagNo`. If `SIZE(d) = 1`
!   then $d_i$ is replaced by $d_1$.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: SetDiag
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP), PARAMETER :: base = 1.0_DFP
! REAL(DFP) :: mat(n, n), expected(n, n)
! REAL(DFP), ALLOCATABLE :: d(:)
! INTEGER(I4B) :: k, ii, jj
! CHARACTER(LEN=32) :: msg
!
! DO k = -3, 3
!   mat = base
!   d = REAL([(10 + ii, ii=1, n - ABS(k))], DFP)
!
!   CALL SetDiag(mat, d, k)
!
!   expected = base
!   DO jj = 1, n
!     DO ii = 1, n
!       IF (jj - ii .EQ. k) expected(ii, jj) = REAL(10 + MIN(ii, jj), DFP)
!     END DO
!   END DO
!
!   WRITE (msg, "(A,I0)") "SetDiag diagNo = ", k
!   CALL OK(ALL(mat .EQ. expected), TRIM(msg))
! END DO
!
! ! constant diagonal: SIZE(d) == 1
! mat = 0.0_DFP
! CALL SetDiag(mat, [7.0_DFP], 1)
! CALL OK(mat(1, 2) .EQ. 7.0_DFP .AND. mat(4, 5) .EQ. 7.0_DFP &
!         .AND. mat(1, 1) .EQ. 0.0_DFP, "SetDiag constant diagonal")
!
! END PROGRAM main
!```

INTERFACE SetDiag
  MODULE PURE SUBROUTINE SetDiag1(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    INTEGER(INT8), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag1
  MODULE PURE SUBROUTINE SetDiag2(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    INTEGER(INT16), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag2
  MODULE PURE SUBROUTINE SetDiag3(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    INTEGER(INT32), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag3
  MODULE PURE SUBROUTINE SetDiag4(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    INTEGER(INT64), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag4
  MODULE PURE SUBROUTINE SetDiag5(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    REAL(REAL32), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag5
  MODULE PURE SUBROUTINE SetDiag6(mat, d, diagNo)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    REAL(REAL64), INTENT(IN) :: d(:)
    INTEGER(I4B), INTENT(IN) :: diagNo
  END SUBROUTINE SetDiag6
END INTERFACE SetDiag

!----------------------------------------------------------------------------
!                                                                  DiagSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Size of a diagonal of a square matrix.
!
! Description:
!   Returns the number of entries on the `diagNo`-th diagonal of an
!   `n x n` matrix. It returns 0 if the diagonal lies outside the
!   matrix.
!
!   For example, `DiagSize(n=3, diagNo=1)` is the size of the first
!   super-diagonal of a (3, 3) matrix, i.e. 2, and
!   `DiagSize(n=3, diagNo=-1)` is the size of its first sub-diagonal,
!   i.e. 2.
!
! Arguments:
!   n : INTEGER(I4B) - size of the (square) matrix
!   diagNo : INTEGER(I4B) - diagonal number
!
! Returns:
!   ans : INTEGER(I4B) - size of the diagonal
!
! Formula:
!   $$\mathrm{ans} = \max(n - |k|,\, 0)$$
!
!   where $k$ = `diagNo`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: DiagSize
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! square (4,4), DiagSize(n, diagNo)
! CALL OK(DiagSize(4, 0) .EQ. 4, "DiagSize(4, 0)")
! CALL OK(DiagSize(4, 1) .EQ. 3, "DiagSize(4, 1)")
! CALL OK(DiagSize(4, 2) .EQ. 2, "DiagSize(4, 2)")
! CALL OK(DiagSize(4, 3) .EQ. 1, "DiagSize(4, 3)")
! CALL OK(DiagSize(4, 5) .EQ. 0, "DiagSize(4, 5) out of bound")
! CALL OK(DiagSize(4, -1) .EQ. 3, "DiagSize(4, -1)")
! CALL OK(DiagSize(4, -2) .EQ. 2, "DiagSize(4, -2)")
! CALL OK(DiagSize(4, -3) .EQ. 1, "DiagSize(4, -3)")
! CALL OK(DiagSize(4, -5) .EQ. 0, "DiagSize(4, -5) out of bound")
!
! END PROGRAM main
!```

INTERFACE DiagSize
  MODULE PURE FUNCTION DiagSize1(n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
  !! size of matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
  !! diagonal number
    INTEGER(I4B) :: ans
  !! size of diagonal
  END FUNCTION DiagSize1
END INTERFACE DiagSize

!----------------------------------------------------------------------------
!                                                               DiagSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Size of a diagonal of a rectangle matrix.
!
! Description:
!   Returns the number of entries on the `diagNo`-th diagonal of an
!   `m x n` matrix, where `m` is the number of rows and `n` is the
!   number of columns. It returns 0 if the diagonal lies outside the
!   matrix.
!
!   For example, `DiagSize(m=4, n=7, diagNo=1)` is the size of the
!   first super-diagonal of a (4, 7) matrix, i.e. 4.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows
!   n : INTEGER(I4B) - number of columns
!   diagNo : INTEGER(I4B) - diagonal number
!
! Returns:
!   ans : INTEGER(I4B) - size of the diagonal
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!   \min(m,\, n-k) & 0 \le k \le n \\
!   \min(n,\, m+k) & -m \le k < 0 \\
!   0 & \mathrm{otherwise}
!   \end{cases}$$
!
!   where $k$ = `diagNo`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: DiagSize
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! rectangle (4,7), super-diagonals 0..7
! CALL OK(DiagSize(4, 7, 0) .EQ. 4, "DiagSize(4, 7, 0)")
! CALL OK(DiagSize(4, 7, 1) .EQ. 4, "DiagSize(4, 7, 1)")
! CALL OK(DiagSize(4, 7, 2) .EQ. 4, "DiagSize(4, 7, 2)")
! CALL OK(DiagSize(4, 7, 3) .EQ. 4, "DiagSize(4, 7, 3)")
! CALL OK(DiagSize(4, 7, 4) .EQ. 3, "DiagSize(4, 7, 4)")
! CALL OK(DiagSize(4, 7, 5) .EQ. 2, "DiagSize(4, 7, 5)")
! CALL OK(DiagSize(4, 7, 6) .EQ. 1, "DiagSize(4, 7, 6)")
! CALL OK(DiagSize(4, 7, 7) .EQ. 0, "DiagSize(4, 7, 7) out of bound")
!
! ! rectangle (4,7), sub-diagonals
! CALL OK(DiagSize(4, 7, -1) .EQ. 3, "DiagSize(4, 7, -1)")
! CALL OK(DiagSize(4, 7, -2) .EQ. 2, "DiagSize(4, 7, -2)")
! CALL OK(DiagSize(4, 7, -3) .EQ. 1, "DiagSize(4, 7, -3)")
! CALL OK(DiagSize(4, 7, -4) .EQ. 0, "DiagSize(4, 7, -4) out of bound")
!
! ! rectangle (7,4)
! CALL OK(DiagSize(7, 4, 1) .EQ. 3, "DiagSize(7, 4, 1)")
! CALL OK(DiagSize(7, 4, 4) .EQ. 0, "DiagSize(7, 4, 4) out of bound")
! CALL OK(DiagSize(7, 4, -3) .EQ. 4, "DiagSize(7, 4, -3)")
! CALL OK(DiagSize(7, 4, -4) .EQ. 3, "DiagSize(7, 4, -4)")
!
! END PROGRAM main
!```

INTERFACE DiagSize
  MODULE PURE FUNCTION DiagSize2(m, n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    !! number of rows in matrix
    INTEGER(I4B), INTENT(IN) :: n
    !! number of columns in a matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
    !! diagonal number
    INTEGER(I4B) :: ans
    !! size of diagonal
  END FUNCTION DiagSize2
END INTERFACE DiagSize

!----------------------------------------------------------------------------
!                                                               DiagIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Indices of the entries on a diagonal of a matrix.
!
! Description:
!   Returns the indices of the entries along the `diagNo`-th diagonal
!   of a square or rectangle matrix of shape `(m, n)`.
!
!   The indices are returned in a two dimensional integer array `ans`
!   with two columns.
!
!   - The first column `ans(:, 1)` is the row index.
!   - The second column `ans(:, 2)` is the column index.
!   - The number of rows of `ans` is `DiagSize(m, n, diagNo)`. It is
!     zero if the diagonal lies outside the matrix.
!   - The entries are ordered from the top-left to the bottom-right.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows in matrix
!   n : INTEGER(I4B) - number of columns in matrix
!   diagNo : INTEGER(I4B) - diagonal number
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, shape (DiagSize(m, n, diagNo), 2)
!
! Formula:
!   $$(\mathrm{ans}_{l,1},\, \mathrm{ans}_{l,2}) = \begin{cases}
!   (l,\, l+k) & k \ge 0 \\
!   (l-k,\, l) & k < 0
!   \end{cases}, \qquad 1 \le l \le \mathrm{DiagSize}(m, n, k)$$
!
!   where $k$ = `diagNo`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: DiagIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! (4,4)
! CALL OK(isEqual(DiagIndx(4, 4, 0), &
!                 RESHAPE([1, 2, 3, 4, 1, 2, 3, 4], [4, 2])), &
!         "DiagIndx(4,4, 0)")
! CALL OK(isEqual(DiagIndx(4, 4, 1), &
!                 RESHAPE([1, 2, 3, 2, 3, 4], [3, 2])), &
!         "DiagIndx(4,4, 1)")
! CALL OK(isEqual(DiagIndx(4, 4, 2), &
!                 RESHAPE([1, 2, 3, 4], [2, 2])), &
!         "DiagIndx(4,4, 2)")
! CALL OK(isEqual(DiagIndx(4, 4, -1), &
!                 RESHAPE([2, 3, 4, 1, 2, 3], [3, 2])), &
!         "DiagIndx(4,4, -1)")
! CALL OK(isEqual(DiagIndx(4, 4, -2), &
!                 RESHAPE([3, 4, 1, 2], [2, 2])), &
!         "DiagIndx(4,4, -2)")
!
! ! (4,7)
! CALL OK(isEqual(DiagIndx(4, 7, 3), &
!                 RESHAPE([1, 2, 3, 4, 4, 5, 6, 7], [4, 2])), &
!         "DiagIndx(4,7, 3)")
! CALL OK(isEqual(DiagIndx(4, 7, 4), &
!                 RESHAPE([1, 2, 3, 5, 6, 7], [3, 2])), &
!         "DiagIndx(4,7, 4)")
! CALL OK(isEqual(DiagIndx(4, 7, 6), &
!                 RESHAPE([1, 7], [1, 2])), &
!         "DiagIndx(4,7, 6)")
! CALL OK(isEqual(DiagIndx(4, 7, -1), &
!                 RESHAPE([2, 3, 4, 1, 2, 3], [3, 2])), &
!         "DiagIndx(4,7, -1)")
! CALL OK(isEqual(DiagIndx(4, 7, -3), &
!                 RESHAPE([4, 1], [1, 2])), &
!         "DiagIndx(4,7, -3)")
!
! ! out of bound diagonals have no entries
! CALL OK(SIZE(DiagIndx(4, 7, 7), 1) .EQ. 0, "DiagIndx(4,7, 7) is empty")
! CALL OK(SIZE(DiagIndx(4, 7, -4), 1) .EQ. 0, "DiagIndx(4,7, -4) is empty")
!
! CONTAINS
!
! PURE FUNCTION isEqual(a, b) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: a(:, :), b(:, :)
!   LOGICAL :: ans
!   ans = ALL(SHAPE(a) .EQ. SHAPE(b))
!   IF (ans) ans = ALL(a .EQ. b)
! END FUNCTION isEqual
!
! END PROGRAM main
!```

INTERFACE DiagIndx
  MODULE PURE FUNCTION DiagIndx(m, n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    !! number of rows in matrix
    INTEGER(I4B), INTENT(IN) :: n
    !! number of columns in a matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
    !! diagonal number
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! size of diagonal
  END FUNCTION DiagIndx
END INTERFACE DiagIndx

!----------------------------------------------------------------------------
!                                                               SetTriDiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Set the three diagonals of a tridiagonal matrix.
!
! Description:
!   Sets the main diagonal, the first super-diagonal and the first
!   sub-diagonal of the square matrix `mat`. The other entries of `mat`
!   are not modified.
!
!   - `d` denotes the main diagonal.
!   - `da` denotes the super-diagonal (a for above).
!   - `db` denotes the sub-diagonal (b for below).
!
!   `d`, `da`, `db` are vectors of the same kind, which can be `Int8`,
!   `Int16`, `Int32`, `Int64`, `Real32` or `Real64`.
!
!   - If `SIZE(d/da/db) = 1`, then all entries of the respective
!     diagonal are set to this value.
!   - If `SIZE(d/da/db) .NE. 1`, then the size of `d/da/db` should be
!     at least the size of the respective diagonal, i.e., `n` for `d`
!     and `n - 1` for `da` and `db`.
!
!   Sizes are not checked at run time.
!
! Arguments:
!   mat : REAL(DFP), rank 2, INOUT - square matrix of shape (n, n)
!   d : main diagonal, size 1 or at least n
!   da : first super-diagonal, size 1 or at least n - 1
!   db : first sub-diagonal, size 1 or at least n - 1
!
! Formula:
!   $$A_{ii} \leftarrow d_i, \qquad
!   A_{i,\,i+1} \leftarrow da_i, \qquad
!   A_{i+1,\,i} \leftarrow db_i$$
!
!   for the valid range of $i$. If the size of a vector is 1 then its
!   entries are replaced by its first entry.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: SetTriDiag
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP), PARAMETER :: base = 7.0_DFP
! REAL(DFP) :: mat(n, n), expected(n, n)
! REAL(DFP) :: d(n), da(n - 1), db(n - 1)
! INTEGER(I4B) :: ii
!
! DO ii = 1, n
!   d(ii) = REAL(10 + ii, DFP)
! END DO
! DO ii = 1, n - 1
!   da(ii) = REAL(20 + ii, DFP)
!   db(ii) = REAL(30 + ii, DFP)
! END DO
!
! mat = base
! CALL SetTriDiag(mat=mat, d=d, da=da, db=db)
!
! expected = base
! DO ii = 1, n
!   expected(ii, ii) = d(ii)
! END DO
! DO ii = 1, n - 1
!   expected(ii, ii + 1) = da(ii)
!   expected(ii + 1, ii) = db(ii)
! END DO
!
! CALL OK(ALL(mat .EQ. expected), "SetTriDiag values")
!
! ! entries away from the three diagonals are untouched
! CALL OK(mat(1, 3) .EQ. base .AND. mat(3, 1) .EQ. base &
!         .AND. mat(1, 5) .EQ. base .AND. mat(5, 1) .EQ. base, &
!         "SetTriDiag untouched entries")
!
! ! the super-diagonal goes to (i, i+1), the sub-diagonal to (i+1, i)
! CALL OK(mat(1, 2) .EQ. 21.0_DFP .AND. mat(2, 1) .EQ. 31.0_DFP, &
!         "SetTriDiag da is above, db is below")
!
! END PROGRAM main
!```

INTERFACE SetTriDiag
  MODULE PURE SUBROUTINE SetTriDiag1(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    INTEGER(INT8), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT8), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    INTEGER(INT8), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag1

  MODULE PURE SUBROUTINE SetTriDiag2(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    INTEGER(INT16), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT16), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    INTEGER(INT16), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag2

  MODULE PURE SUBROUTINE SetTriDiag3(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    INTEGER(INT32), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT32), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    INTEGER(INT32), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag3

  MODULE PURE SUBROUTINE SetTriDiag4(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    INTEGER(INT64), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT64), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    INTEGER(INT64), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag4

  MODULE PURE SUBROUTINE SetTriDiag5(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    REAL(REAL32), INTENT(IN) :: d(:)
    !! main diagonal
    REAL(REAL32), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    REAL(REAL32), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag5

  MODULE PURE SUBROUTINE SetTriDiag6(mat, d, da, db)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    !! tri diagonal matrix dense form
    REAL(REAL64), INTENT(IN) :: d(:)
    !! main diagonal
    REAL(REAL64), INTENT(IN) :: da(:)
    !! super-diagonal, (a, for above)
    REAL(REAL64), INTENT(IN) :: db(:)
    !! sub-diagonal (b for below)
  END SUBROUTINE SetTriDiag6
END INTERFACE SetTriDiag

!----------------------------------------------------------------------------
!                                                                   Tridiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Make a tridiagonal matrix from main, sub, super diagonal.
!
! Description:
!   Creates a square matrix with the main diagonal `d`, the
!   super-diagonal `da` and the sub-diagonal `db`. All other entries are
!   zero.
!
!   - `d`, `da`, `db` are vectors of the same kind, which can be `Int8`,
!     `Int16`, `Int32`, `Int64`, `Real32` or `Real64`.
!   - `diagNo` is the number of the super and sub-diagonal. It is
!     optional and its default value is 1, which gives a standard
!     tridiagonal matrix. Larger values give a "banded" matrix with
!     the two diagonals shifted away from the main diagonal.
!     `diagNo` should be positive; its absolute value is used.
!   - If `SIZE(da/db) = 1`, all entries of the diagonal are set to
!     this value, otherwise the size of `da` and `db` should be at least
!     `n - diagNo`.
!   - The result is `n x n` matrix of `REAL(DFP)`, where `n = SIZE(d)`.
!
! Arguments:
!   d : main diagonal, size n
!   da : super diagonal, size 1 or at least n - diagNo
!   db : sub diagonal, size 1 or at least n - diagNo
!   diagNo : INTEGER(I4B), OPTIONAL - diagonal number, default is 1
!
! Returns:
!   ans : REAL(DFP), shape (SIZE(d), SIZE(d))
!
! Formula:
!   $$\mathrm{ans}_{ii} = d_i, \qquad
!   \mathrm{ans}_{i,\,i+k} = da_i, \qquad
!   \mathrm{ans}_{i+k,\,i} = db_i$$
!
!   where $k$ = `diagNo`, and all other entries are zero.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE DiagUtility, ONLY: TriDiag
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP) :: d(n), da(n - 1), db(n - 1)
! REAL(DFP) :: amat(n, n), expected(n, n)
!
! d = 1.0_DFP
! da = 2.0_DFP
! db = -2.0_DFP
!
! ! RESHAPE fills column by column, so the rows written below are
! ! transposed to get the matrix in the natural (row-wise) reading order
! expected = TRANSPOSE(RESHAPE([ &
!                       1, 2, 0, 0, 0, &
!                       -2, 1, 2, 0, 0, &
!                       0, -2, 1, 2, 0, &
!                       0, 0, -2, 1, 2, &
!                       0, 0, 0, -2, 1], [n, n]))
!
! amat = TriDiag(d=d, da=da, db=db, diagNo=1_I4B)
! CALL OK(ALL(SHAPE(amat) .EQ. [n, n]), "TriDiag shape")
! CALL OK(ALL(amat .EQ. expected), "TriDiag diagNo = 1")
!
! amat = TriDiag(d=d, da=da, db=db)
! CALL OK(ALL(amat .EQ. expected), "TriDiag default diagNo")
!
! END PROGRAM main
!```

INTERFACE Tridiag
  MODULE PURE FUNCTION Tridiag_1(d, da, db, diagNo) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT8), INTENT(IN) :: da(:)
    !! super diagonal
    INTEGER(INT8), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_1

  MODULE PURE FUNCTION Tridiag_2(d, da, db, diagNo) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT16), INTENT(IN) :: da(:)
    !! super diagonal
    INTEGER(INT16), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_2

  MODULE PURE FUNCTION Tridiag_3(d, da, db, diagNo) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT32), INTENT(IN) :: da(:)
    !! super diagonal
    INTEGER(INT32), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_3

  MODULE PURE FUNCTION Tridiag_4(d, da, db, diagNo) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: d(:)
    !! main diagonal
    INTEGER(INT64), INTENT(IN) :: da(:)
    !! super diagonal
    INTEGER(INT64), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_4

  MODULE PURE FUNCTION Tridiag_5(d, da, db, diagNo) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: d(:)
    !! main diagonal
    REAL(REAL32), INTENT(IN) :: da(:)
    !! super diagonal
    REAL(REAL32), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_5

  MODULE PURE FUNCTION Tridiag_6(d, da, db, diagNo) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: d(:)
    !! main diagonal
    REAL(REAL64), INTENT(IN) :: da(:)
    !! super diagonal
    REAL(REAL64), INTENT(IN) :: db(:)
    !! subdiagonal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! sub and super diagonal number, default is 1
    !! diagNo should be positive
    REAL(DFP) :: ans(SIZE(d), SIZE(d))
  END FUNCTION Tridiag_6

END INTERFACE Tridiag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DiagUtility
