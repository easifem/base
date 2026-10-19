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
! date: 2022-12-21
! summary: Get and set the upper and lower triangular parts of a matrix
!
! Description:
!   This module provides routines to get and set the upper and lower
!   triangular parts of a square or rectangular matrix. It is inspired by
!   the same functionality available in numpy (`triu`, `tril`,
!   `triu_indices`, `tril_indices`).
!
!   - `TriuIndx`, `TrilIndx` return the (row, column) indices of the
!     upper or lower triangle.
!   - `Triu`, `Tril` return the triangle, either as a full matrix with
!     zeros elsewhere, or (with `flate`) as a vector.
!   - `GetTriu`, `GetTril` do the same as subroutines.
!   - `SetTriu`, `SetTril` set the triangle of a matrix from a matrix,
!     from a vector, or to a scalar value.
!
!   The diagonal number `diagNo` (default 0) selects where the triangle
!   starts:
!
!   - `diagNo = 0` is the main diagonal,
!   - `diagNo > 0` is a super-diagonal (above the main diagonal),
!   - `diagNo < 0` is a sub-diagonal (below the main diagonal).
!
!   The upper triangle keeps the entries with `j - i >= diagNo`, and the
!   lower triangle keeps the entries with `j - i <= diagNo`. A `diagNo`
!   outside the matrix is allowed: the triangle is then empty or the
!   whole matrix.
!
!   Order of the entries: the index arrays and the vectors list the
!   entries diagonal by diagonal, not row by row as numpy does. The
!   upper triangle starts at diagonal `diagNo` and moves up to the last
!   super-diagonal; the lower triangle starts at diagonal `diagNo` and
!   moves down to the last sub-diagonal. Within a diagonal the entries
!   go from the top-left to the bottom-right. For example, the upper
!   triangle of a (3,3) matrix with `diagNo = 0` is listed as
!   (1,1), (2,2), (3,3), (1,2), (2,3), (1,3).
!
!```fortran
! indx = TriuIndx(m=3, n=5, diagNo=0) ! (3,5) matrix, main diagonal
! indx = TriuIndx(m=4)                ! (4,4) matrix, diagNo = 0
! indx = TrilIndx(A, diagNo=-1)       ! shape of A, first sub-diagonal
! u = Triu(A, diagNo=1)               ! full matrix, zeros elsewhere
! v = Tril(A, flate=.TRUE.)           ! vector, diagonal by diagonal
! CALL SetTriu(A=B, val=0.0_DFP, diagNo=1)
!```

MODULE TriagUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: TriuIndx
PUBLIC :: TrilIndx
PUBLIC :: Tril
PUBLIC :: Triu
PUBLIC :: GetTril
PUBLIC :: GetTriu
PUBLIC :: SetTril
PUBLIC :: SetTriu

!----------------------------------------------------------------------------
!                                                                    TriuIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Indices of the upper triangle of an (m, n) matrix
!
! Description:
!   Returns the (row, column) indices of the entries of an (m, n) matrix
!   with `j - i >= diagNo`. The row indices are in `ans(:, 1)` and the
!   column indices are in `ans(:, 2)`.
!
!   - The entries are listed diagonal by diagonal, starting at diagonal
!     `diagNo` and moving up to the last super-diagonal `n - 1`. Within a
!     diagonal they go from the top-left to the bottom-right.
!   - `diagNo >= n` gives an empty triangle and `diagNo <= -(m - 1)`
!     gives all `m * n` entries.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows
!   n : INTEGER(I4B), OPTIONAL - number of columns, default `m`
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - the indices,
!     `SIZE(ans, 2) = 2`, `SIZE(ans, 1)` is the number of entries
!
! Formula:
!   $$\{(i, j) : 1 \le i \le m,\ 1 \le j \le n,\ j - i \ge k\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: TriuIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: indx(:, :)
!
! ! square (3,3), main diagonal and above, diagonal by diagonal
! indx = TriuIndx(m=3_I4B, n=3_I4B, diagNo=0_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 6_I4B, "(3,3) diagNo=0: size")
! CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 3, 1, 2, 1]), "(3,3) diagNo=0: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 3, 2, 3, 3]), "(3,3) diagNo=0: cols")
!
! ! default n = m and diagNo = 0
! CALL OK(ALL(TriuIndx(m=3_I4B) .EQ. indx), "default n and diagNo")
!
! ! first super-diagonal and above
! indx = TriuIndx(m=3_I4B, n=3_I4B, diagNo=1_I4B)
! CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 1]), "(3,3) diagNo=1: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [2, 3, 3]), "(3,3) diagNo=1: cols")
!
! ! first sub-diagonal and above
! indx = TriuIndx(m=3_I4B, n=3_I4B, diagNo=-1_I4B)
! CALL OK(ALL(indx(:, 1) .EQ. [2, 3, 1, 2, 3, 1, 2, 1]), &
!         "(3,3) diagNo=-1: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 1, 2, 3, 2, 3, 3]), &
!         "(3,3) diagNo=-1: cols")
!
! ! diagNo outside the matrix
! CALL OK(SIZE(TriuIndx(3_I4B, 3_I4B, 3_I4B), 1) .EQ. 0_I4B, &
!         "(3,3) diagNo=3: empty")
! CALL OK(SIZE(TriuIndx(3_I4B, 3_I4B, -5_I4B), 1) .EQ. 9_I4B, &
!         "(3,3) diagNo=-5: all entries")
!
! ! rectangular matrices
! indx = TriuIndx(m=3_I4B, n=5_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 12_I4B, "(3,5) diagNo=0: size")
! CALL OK(ALL(indx(:, 2) - indx(:, 1) .GE. 0), "(3,5) j - i >= 0")
! CALL OK(ALL(indx(12, :) .EQ. [1, 5]), "(3,5) last entry is (1,5)")
!
! indx = TriuIndx(m=5_I4B, n=3_I4B, diagNo=-1_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 9_I4B, "(5,3) diagNo=-1: size")
! CALL OK(ALL(indx(1:3, 1) .EQ. [2, 3, 4]), "(5,3) diagNo=-1: rows")
! CALL OK(ALL(indx(1:3, 2) .EQ. [1, 2, 3]), "(5,3) diagNo=-1: cols")
!
! END PROGRAM main
!```

INTERFACE TriuIndx
  MODULE PURE FUNCTION TriuIndx_1(m, n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    !! number of rows
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
    !! number of columns, default = m
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION TriuIndx_1
END INTERFACE TriuIndx

!----------------------------------------------------------------------------
!                                                                    TriuIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Indices of the upper triangle of a matrix
!
! Description:
!   Returns `TriuIndx(SIZE(A, 1), SIZE(A, 2), diagNo)`. Only the shape of
!   `A` is used; `A` can be of any type and its values are not read.
!
! Arguments:
!   A : CLASS(*), rank(2) - matrix whose shape is used
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - the indices, see
!     `TriuIndx(m, n, diagNo)`; `ans(:, 1)` rows, `ans(:, 2)` columns
!
! Formula:
!   $$\{(i, j) : j - i \ge k\}, \quad
!     (m, n) = \mathrm{SHAPE}(A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: TriuIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3, 5)
! INTEGER(I4B) :: b(5, 3)
!
! a = 0.0_DFP
! b = 0_I4B
!
! ! only the shape of A is used
! CALL OK(ALL(TriuIndx(a) .EQ. TriuIndx(3_I4B, 5_I4B)), &
!         "real (3,5), default diagNo")
! CALL OK(ALL(TriuIndx(a, diagNo=1_I4B) .EQ. &
!             TriuIndx(3_I4B, 5_I4B, 1_I4B)), "real (3,5), diagNo=1")
! CALL OK(ALL(TriuIndx(b, diagNo=-1_I4B) .EQ. &
!             TriuIndx(5_I4B, 3_I4B, -1_I4B)), "integer (5,3), diagNo=-1")
!
! ! the values of A do not matter
! a = 7.0_DFP
! CALL OK(ALL(TriuIndx(a) .EQ. TriuIndx(3_I4B, 5_I4B)), "values ignored")
! CALL OK(SIZE(TriuIndx(a), 1) .EQ. 12_I4B, "real (3,5): size")
!
! END PROGRAM main
!```

INTERFACE TriuIndx
  MODULE PURE FUNCTION TriuIndx_2(A, diagNo) RESULT(ans)
    CLASS(*), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION TriuIndx_2
END INTERFACE TriuIndx

!----------------------------------------------------------------------------
!                                                                    TrilIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Indices of the lower triangle of an (m, n) matrix
!
! Description:
!   Returns the (row, column) indices of the entries of an (m, n) matrix
!   with `j - i <= diagNo`. The row indices are in `ans(:, 1)` and the
!   column indices are in `ans(:, 2)`.
!
!   - The entries are listed diagonal by diagonal, starting at diagonal
!     `diagNo` and moving down to the last sub-diagonal `-(m - 1)`. Within a
!     diagonal they go from the top-left to the bottom-right.
!   - `diagNo <= -m` gives an empty triangle and `diagNo >= n - 1`
!     gives all `m * n` entries.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows
!   n : INTEGER(I4B), OPTIONAL - number of columns, default `m`
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - the indices,
!     `SIZE(ans, 2) = 2`, `SIZE(ans, 1)` is the number of entries
!
! Formula:
!   $$\{(i, j) : 1 \le i \le m,\ 1 \le j \le n,\ j - i \le k\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: TrilIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: indx(:, :)
!
! ! square (3,3), main diagonal and below, diagonal by diagonal
! indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=0_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 6_I4B, "(3,3) diagNo=0: size")
! CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 3, 2, 3, 3]), "(3,3) diagNo=0: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 3, 1, 2, 1]), "(3,3) diagNo=0: cols")
!
! ! default n = m and diagNo = 0
! CALL OK(ALL(TrilIndx(m=3_I4B) .EQ. indx), "default n and diagNo")
!
! ! first super-diagonal and below
! indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=1_I4B)
! CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 1, 2, 3, 2, 3, 3]), &
!         "(3,3) diagNo=1: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [2, 3, 1, 2, 3, 1, 2, 1]), &
!         "(3,3) diagNo=1: cols")
!
! ! first sub-diagonal and below
! indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=-1_I4B)
! CALL OK(ALL(indx(:, 1) .EQ. [2, 3, 3]), "(3,3) diagNo=-1: rows")
! CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 1]), "(3,3) diagNo=-1: cols")
!
! ! diagNo outside the matrix
! CALL OK(SIZE(TrilIndx(3_I4B, 3_I4B, -3_I4B), 1) .EQ. 0_I4B, &
!         "(3,3) diagNo=-3: empty")
! CALL OK(SIZE(TrilIndx(3_I4B, 3_I4B, 5_I4B), 1) .EQ. 9_I4B, &
!         "(3,3) diagNo=5: all entries")
!
! ! rectangular matrices
! indx = TrilIndx(m=3_I4B, n=5_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 6_I4B, "(3,5) diagNo=0: size")
! CALL OK(ALL(indx(:, 2) - indx(:, 1) .LE. 0), "(3,5) j - i <= 0")
!
! indx = TrilIndx(m=5_I4B, n=3_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 12_I4B, "(5,3) diagNo=0: size")
! CALL OK(ALL(indx(12, :) .EQ. [5, 1]), "(5,3) last entry is (5,1)")
!
! indx = TrilIndx(m=3_I4B, n=5_I4B, diagNo=1_I4B)
! CALL OK(SIZE(indx, 1) .EQ. 9_I4B, "(3,5) diagNo=1: size")
!
! END PROGRAM main
!```

INTERFACE TrilIndx
  MODULE PURE FUNCTION TrilIndx_1(m, n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    !! number of rows
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
    !! number of columns, default = m
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! ans(:,1) contains the row indices
    !! ans(:,2) contains the col indices
  END FUNCTION TrilIndx_1
END INTERFACE TrilIndx

!----------------------------------------------------------------------------
!                                                                    TrilIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Indices of the lower triangle of a matrix
!
! Description:
!   Returns `TrilIndx(SIZE(A, 1), SIZE(A, 2), diagNo)`. Only the shape of
!   `A` is used; `A` can be of any type and its values are not read.
!
! Arguments:
!   A : CLASS(*), rank(2) - matrix whose shape is used
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, rank(2) - the indices, see
!     `TrilIndx(m, n, diagNo)`; `ans(:, 1)` rows, `ans(:, 2)` columns
!
! Formula:
!   $$\{(i, j) : j - i \le k\}, \quad
!     (m, n) = \mathrm{SHAPE}(A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: TrilIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3, 5)
! INTEGER(I4B) :: b(5, 3)
!
! a = 0.0_DFP
! b = 0_I4B
!
! ! only the shape of A is used
! CALL OK(ALL(TrilIndx(a) .EQ. TrilIndx(3_I4B, 5_I4B)), &
!         "real (3,5), default diagNo")
! CALL OK(ALL(TrilIndx(a, diagNo=1_I4B) .EQ. &
!             TrilIndx(3_I4B, 5_I4B, 1_I4B)), "real (3,5), diagNo=1")
! CALL OK(ALL(TrilIndx(b, diagNo=-1_I4B) .EQ. &
!             TrilIndx(5_I4B, 3_I4B, -1_I4B)), "integer (5,3), diagNo=-1")
!
! ! the values of A do not matter
! b = 7_I4B
! CALL OK(ALL(TrilIndx(b) .EQ. TrilIndx(5_I4B, 3_I4B)), "values ignored")
! CALL OK(SIZE(TrilIndx(b), 1) .EQ. 12_I4B, "integer (5,3): size")
!
! END PROGRAM main
!```

INTERFACE TrilIndx
  MODULE PURE FUNCTION TrilIndx_2(A, diagNo) RESULT(ans)
    CLASS(*), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! ans(:,1) contains the row indices
    !! ans(:,2) contains the col indices
  END FUNCTION TrilIndx_2
END INTERFACE TrilIndx

!----------------------------------------------------------------------------
!                                                                        Triu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangular part of a matrix
!
! Description:
!   Returns a matrix of the same shape as `A` that keeps the entries
!   with `j - i >= diagNo` and sets all other entries to zero.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : same type and kind as `A`, `SHAPE(ans) = SHAPE(A)`
!
! Formula:
!   $$u_{ij} = \begin{cases}
!     a_{ij}, & j - i \ge k \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE TriagUtility, ONLY: Triu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), u(3, 3), i, j
! REAL(DFP) :: b(3, 5), v(3, 5), c(5, 3), w(5, 3)
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! integer (3,3), default diagNo = 0
! u = Triu(a)
! CALL OK(ALL(u .EQ. RESHAPE([11, 0, 0, 12, 22, 0, 13, 23, 33], &
!                            [3, 3])), "integer, diagNo=0")
!
! ! diagNo = 1 and diagNo = -1
! u = Triu(a, diagNo=1_I4B)
! CALL OK(ALL(u .EQ. RESHAPE([0, 0, 0, 12, 0, 0, 13, 23, 0], &
!                            [3, 3])), "integer, diagNo=1")
! u = Triu(a, diagNo=-1_I4B)
! CALL OK(ALL(u .EQ. RESHAPE([11, 21, 0, 12, 22, 32, 13, 23, 33], &
!                            [3, 3])), "integer, diagNo=-1")
!
! ! real rectangular matrices
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
! c = TRANSPOSE(b)
!
! v = Triu(b)
! CALL OK(IsPart(b, v, 0_I4B), "real (3,5), diagNo=0")
! v = Triu(b, diagNo=2_I4B)
! CALL OK(IsPart(b, v, 2_I4B), "real (3,5), diagNo=2")
! w = Triu(c, diagNo=-1_I4B)
! CALL OK(IsPart(c, w, -1_I4B), "real (5,3), diagNo=-1")
!
! CONTAINS
!
! ! true if u keeps a where j - i .GE. k and is zero elsewhere
! PURE FUNCTION IsPart(a, u, k) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: a(:, :)
!   REAL(DFP), INTENT(IN) :: u(:, :)
!   INTEGER(I4B), INTENT(IN) :: k
!   LOGICAL(LGT) :: ans
!   INTEGER(I4B) :: i, j
!   REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
!   ans = .TRUE.
!   DO j = 1, SIZE(a, 2)
!     DO i = 1, SIZE(a, 1)
!       IF (j - i .GE. k) THEN
!         ans = ans .AND. ABS(u(i, j) - a(i, j)) .LT. tol
!       ELSE
!         ans = ans .AND. ABS(u(i, j)) .LT. tol
!       END IF
!     END DO
!   END DO
! END FUNCTION IsPart
!
! END PROGRAM main
!```

INTERFACE Triu
  MODULE PURE FUNCTION Triu_1(A, diagNo) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL32) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_1

  MODULE PURE FUNCTION Triu_2(A, diagNo) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL64) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_2

  MODULE PURE FUNCTION Triu_3(A, diagNo) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT8) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_3

  MODULE PURE FUNCTION Triu_4(A, diagNo) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT16) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_4

  MODULE PURE FUNCTION Triu_5(A, diagNo) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT32) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_5

  MODULE PURE FUNCTION Triu_6(A, diagNo) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT64) :: ans(SIZE(A, 1), SIZE(A, 2))
  END FUNCTION Triu_6
END INTERFACE Triu

!----------------------------------------------------------------------------
!                                                                        Triu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangular part of a matrix as a vector
!
! Description:
!   Returns the entries of `A` with `j - i >= diagNo` as a vector,
!   in the order of `TriuIndx(A, diagNo)` (diagonal by diagonal).
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - `flate` only selects this version of `Triu`; its value is not
!     used, so `flate = .FALSE.` also returns a vector.
!   - The result is empty when the triangle is empty.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : same type and kind as `A`, ALLOCATABLE, rank(1),
!     `SIZE(ans) = SIZE(TriuIndx(A, diagNo), 1)`
!
! Formula:
!   $$v_p = a_{i_p j_p}, \quad (i_p, j_p) = TriuIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE TriagUtility, ONLY: Triu
! USE TriagUtility, ONLY: TriuIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), i, j
! INTEGER(I4B), ALLOCATABLE :: v(:), indx(:, :)
! REAL(DFP) :: b(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! LOGICAL(LGT) :: isok
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! diagonal by diagonal: main diagonal first, then the super-diagonals
! v = Triu(a, flate=.TRUE.)
! CALL OK(ALL(v .EQ. [11, 22, 33, 12, 23, 13]), "integer, diagNo=0")
!
! v = Triu(a, flate=.TRUE., diagNo=1_I4B)
! CALL OK(ALL(v .EQ. [12, 23, 13]), "integer, diagNo=1")
!
! v = Triu(a, flate=.TRUE., diagNo=-1_I4B)
! CALL OK(ALL(v .EQ. [21, 32, 11, 22, 33, 12, 23, 13]), &
!         "integer, diagNo=-1")
!
! ! the value of flate is not used
! v = Triu(a, flate=.FALSE.)
! CALL OK(ALL(v .EQ. [11, 22, 33, 12, 23, 13]), "flate=.FALSE. is the same")
!
! ! a diagonal outside the matrix gives an empty vector
! v = Triu(a, flate=.TRUE., diagNo=3_I4B)
! CALL OK(SIZE(v) .EQ. 0_I4B, "integer, diagNo=3: empty")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! real (3,5): the order is the order of TriuIndx
! w = Triu(b, flate=.TRUE., diagNo=-1_I4B)
! indx = TriuIndx(b, diagNo=-1_I4B)
! CALL OK(SIZE(w) .EQ. 14_I4B, "real (3,5), diagNo=-1: size")
! isok = .TRUE.
! DO i = 1, SIZE(w)
!   isok = isok .AND. ABS(w(i) - b(indx(i, 1), indx(i, 2))) .LT. tol
! END DO
! CALL OK(isok, "real (3,5): same order as TriuIndx")
!
! END PROGRAM main
!```

INTERFACE Triu
  MODULE PURE FUNCTION Triu_7(A, flate, diagNo) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL32), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_7
  MODULE PURE FUNCTION Triu_8(A, flate, diagNo) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL64), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_8
  MODULE PURE FUNCTION Triu_9(A, flate, diagNo) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT8), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_9
  MODULE PURE FUNCTION Triu_10(A, flate, diagNo) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT16), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_10
  MODULE PURE FUNCTION Triu_11(A, flate, diagNo) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT32), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_11
  MODULE PURE FUNCTION Triu_12(A, flate, diagNo) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT64), ALLOCATABLE :: ans(:)
  END FUNCTION Triu_12
END INTERFACE Triu

!----------------------------------------------------------------------------
!                                                                        Tril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the lower triangular part of a matrix
!
! Description:
!   Returns a matrix of the same shape as `A` that keeps the entries
!   with `j - i <= diagNo` and sets all other entries to zero.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : same type and kind as `A`, `SHAPE(ans) = SHAPE(A)`
!
! Formula:
!   $$u_{ij} = \begin{cases}
!     a_{ij}, & j - i \le k \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE TriagUtility, ONLY: Tril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), u(3, 3), i, j
! REAL(DFP) :: b(3, 5), v(3, 5), c(5, 3), w(5, 3)
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! integer (3,3), default diagNo = 0
! u = Tril(a)
! CALL OK(ALL(u .EQ. RESHAPE([11, 21, 31, 0, 22, 32, 0, 0, 33], &
!                            [3, 3])), "integer, diagNo=0")
!
! ! diagNo = 1 and diagNo = -1
! u = Tril(a, diagNo=1_I4B)
! CALL OK(ALL(u .EQ. RESHAPE([11, 21, 31, 12, 22, 32, 0, 23, 33], &
!                            [3, 3])), "integer, diagNo=1")
! u = Tril(a, diagNo=-1_I4B)
! CALL OK(ALL(u .EQ. RESHAPE([0, 21, 31, 0, 0, 32, 0, 0, 0], &
!                            [3, 3])), "integer, diagNo=-1")
!
! ! real rectangular matrices
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
! c = TRANSPOSE(b)
!
! v = Tril(b)
! CALL OK(IsPart(b, v, 0_I4B), "real (3,5), diagNo=0")
! v = Tril(b, diagNo=2_I4B)
! CALL OK(IsPart(b, v, 2_I4B), "real (3,5), diagNo=2")
! w = Tril(c, diagNo=-1_I4B)
! CALL OK(IsPart(c, w, -1_I4B), "real (5,3), diagNo=-1")
!
! CONTAINS
!
! ! true if u keeps a where j - i .LE. k and is zero elsewhere
! PURE FUNCTION IsPart(a, u, k) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: a(:, :)
!   REAL(DFP), INTENT(IN) :: u(:, :)
!   INTEGER(I4B), INTENT(IN) :: k
!   LOGICAL(LGT) :: ans
!   INTEGER(I4B) :: i, j
!   REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
!   ans = .TRUE.
!   DO j = 1, SIZE(a, 2)
!     DO i = 1, SIZE(a, 1)
!       IF (j - i .LE. k) THEN
!         ans = ans .AND. ABS(u(i, j) - a(i, j)) .LT. tol
!       ELSE
!         ans = ans .AND. ABS(u(i, j)) .LT. tol
!       END IF
!     END DO
!   END DO
! END FUNCTION IsPart
!
! END PROGRAM main
!```

INTERFACE Tril
  MODULE PURE FUNCTION Tril_1(A, diagNo) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL32) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_1

  MODULE PURE FUNCTION Tril_2(A, diagNo) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL64) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_2

  MODULE PURE FUNCTION Tril_3(A, diagNo) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT8) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_3

  MODULE PURE FUNCTION Tril_4(A, diagNo) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT16) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_4

  MODULE PURE FUNCTION Tril_5(A, diagNo) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT32) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_5

  MODULE PURE FUNCTION Tril_6(A, diagNo) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT64) :: ans(SIZE(A, 1), SIZE(A, 2))
    !! Lower trianglular matrix
  END FUNCTION Tril_6
END INTERFACE Tril

!----------------------------------------------------------------------------
!                                                                        Tril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the lower triangular part of a matrix as a vector
!
! Description:
!   Returns the entries of `A` with `j - i <= diagNo` as a vector,
!   in the order of `TrilIndx(A, diagNo)` (diagonal by diagonal).
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - `flate` only selects this version of `Tril`; its value is not
!     used, so `flate = .FALSE.` also returns a vector.
!   - The result is empty when the triangle is empty.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   ans : same type and kind as `A`, ALLOCATABLE, rank(1),
!     `SIZE(ans) = SIZE(TrilIndx(A, diagNo), 1)`
!
! Formula:
!   $$v_p = a_{i_p j_p}, \quad (i_p, j_p) = TrilIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE TriagUtility, ONLY: Tril
! USE TriagUtility, ONLY: TrilIndx
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), i, j
! INTEGER(I4B), ALLOCATABLE :: v(:), indx(:, :)
! REAL(DFP) :: b(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! LOGICAL(LGT) :: isok
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! diagonal by diagonal: main diagonal first, then the sub-diagonals
! v = Tril(a, flate=.TRUE.)
! CALL OK(ALL(v .EQ. [11, 22, 33, 21, 32, 31]), "integer, diagNo=0")
!
! v = Tril(a, flate=.TRUE., diagNo=-1_I4B)
! CALL OK(ALL(v .EQ. [21, 32, 31]), "integer, diagNo=-1")
!
! v = Tril(a, flate=.TRUE., diagNo=1_I4B)
! CALL OK(ALL(v .EQ. [12, 23, 11, 22, 33, 21, 32, 31]), &
!         "integer, diagNo=1")
!
! ! the value of flate is not used
! v = Tril(a, flate=.FALSE.)
! CALL OK(ALL(v .EQ. [11, 22, 33, 21, 32, 31]), "flate=.FALSE. is the same")
!
! ! a diagonal outside the matrix gives an empty vector
! v = Tril(a, flate=.TRUE., diagNo=-3_I4B)
! CALL OK(SIZE(v) .EQ. 0_I4B, "integer, diagNo=-3: empty")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! real (3,5): the order is the order of TrilIndx
! w = Tril(b, flate=.TRUE., diagNo=1_I4B)
! indx = TrilIndx(b, diagNo=1_I4B)
! CALL OK(SIZE(w) .EQ. 9_I4B, "real (3,5), diagNo=1: size")
! isok = .TRUE.
! DO i = 1, SIZE(w)
!   isok = isok .AND. ABS(w(i) - b(indx(i, 1), indx(i, 2))) .LT. tol
! END DO
! CALL OK(isok, "real (3,5): same order as TrilIndx")
!
! END PROGRAM main
!```

INTERFACE Tril
  MODULE PURE FUNCTION Tril_7(A, flate, diagNo) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL32), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_7
  MODULE PURE FUNCTION Tril_8(A, flate, diagNo) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    REAL(REAL64), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_8
  MODULE PURE FUNCTION Tril_9(A, flate, diagNo) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT8), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_9
  MODULE PURE FUNCTION Tril_10(A, flate, diagNo) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT16), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_10
  MODULE PURE FUNCTION Tril_11(A, flate, diagNo) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT32), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_11
  MODULE PURE FUNCTION Tril_12(A, flate, diagNo) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    !! diagNo>0 means super diagonal
    !! diagNo<0 means subdiagonal
    INTEGER(INT64), ALLOCATABLE :: ans(:)
  END FUNCTION Tril_12
END INTERFACE Tril

!----------------------------------------------------------------------------
!                                                                     GetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Gets the upper triangular part of a matrix
!
! Description:
!   Subroutine version of `Triu(A, diagNo)`. On return `lu` keeps the
!   entries of `A` with `j - i >= diagNo` and every other entry of
!   `lu` is zero.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   The argument order is `(A, diagNo, lu)`, so use keywords when
!   `diagNo` is omitted, e.g. `CALL GetTriu(A=A, lu=lu)`.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   lu : same type and kind as `A`, INTENT(OUT),
!     `SHAPE(lu) = SHAPE(A)`
!
! Formula:
!   $$lu_{ij} = \begin{cases}
!     a_{ij}, & j - i \ge k \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: GetTriu
! USE TriagUtility, ONLY: Triu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), lu(3, 3), i, j
! REAL(DFP) :: b(3, 5), lu2(3, 5)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! entries outside the triangle are set to zero
! lu = 99_I4B
! CALL GetTriu(A=a, lu=lu)
! CALL OK(ALL(lu .EQ. Triu(a)), "integer, default diagNo")
!
! CALL GetTriu(A=a, diagNo=-1_I4B, lu=lu)
! CALL OK(ALL(lu .EQ. RESHAPE([11, 21, 0, 12, 22, 32, 13, 23, 33], &
!                             [3, 3])), "integer, diagNo=-1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! CALL GetTriu(A=b, diagNo=1_I4B, lu=lu2)
! CALL OK(ALL(ABS(lu2 - Triu(b, diagNo=1_I4B)) .LT. tol), &
!         "real (3,5), diagNo=1")
!
! END PROGRAM main
!```

INTERFACE GetTriu
  MODULE PURE SUBROUTINE GetTriu_1(A, diagNo, lu)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_1

  MODULE PURE SUBROUTINE GetTriu_2(A, diagNo, lu)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_2

  MODULE PURE SUBROUTINE GetTriu_3(A, diagNo, lu)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_3

  MODULE PURE SUBROUTINE GetTriu_4(A, diagNo, lu)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_4

  MODULE PURE SUBROUTINE GetTriu_5(A, diagNo, lu)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_5

  MODULE PURE SUBROUTINE GetTriu_6(A, diagNo, lu)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTriu_6
END INTERFACE GetTriu

!----------------------------------------------------------------------------
!                                                                     GetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Gets the upper triangular part of a matrix as a vector
!
! Description:
!   Subroutine version of `Triu(A, flate, diagNo)`. `lu` is allocated
!   to the number of entries with `j - i >= diagNo` and filled in the
!   order of `TriuIndx(A, diagNo)`.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   `flate` only selects this version; its value is not used.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   lu : same type and kind as `A`, ALLOCATABLE, INTENT(OUT), rank(1),
!     `SIZE(lu) = SIZE(TriuIndx(A, diagNo), 1)`
!
! Formula:
!   $$lu_p = a_{i_p j_p}, \quad (i_p, j_p) = TriuIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: GetTriu
! USE TriagUtility, ONLY: Triu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), i, j
! INTEGER(I4B), ALLOCATABLE :: v(:)
! REAL(DFP) :: b(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! lu is reallocated to the number of entries
! ALLOCATE (v(2))
! CALL GetTriu(A=a, flate=.TRUE., lu=v)
! CALL OK(SIZE(v) .EQ. 6_I4B, "integer, diagNo=0: size")
! CALL OK(ALL(v .EQ. [11, 22, 33, 12, 23, 13]), "integer, diagNo=0")
!
! CALL GetTriu(A=a, flate=.TRUE., diagNo=1_I4B, lu=v)
! CALL OK(ALL(v .EQ. [12, 23, 13]), "integer, diagNo=1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! same result as Triu with flate
! CALL GetTriu(A=b, flate=.TRUE., diagNo=-1_I4B, lu=w)
! CALL OK(SIZE(w) .EQ. 14_I4B, "real (3,5), diagNo=-1: size")
! CALL OK(ALL(ABS(w - Triu(b, flate=.TRUE., diagNo=-1_I4B)) .LT. tol), &
!         "real (3,5), same as Triu")
!
! END PROGRAM main
!```

INTERFACE GetTriu
  MODULE PURE SUBROUTINE GetTriu_7(A, flate, diagNo, lu)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_7
  MODULE PURE SUBROUTINE GetTriu_8(A, flate, diagNo, lu)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_8
  MODULE PURE SUBROUTINE GetTriu_9(A, flate, diagNo, lu)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_9
  MODULE PURE SUBROUTINE GetTriu_10(A, flate, diagNo, lu)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_10
  MODULE PURE SUBROUTINE GetTriu_11(A, flate, diagNo, lu)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_11
  MODULE PURE SUBROUTINE GetTriu_12(A, flate, diagNo, lu)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTriu_12
END INTERFACE GetTriu

!----------------------------------------------------------------------------
!                                                                     GetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Gets the lower triangular part of a matrix
!
! Description:
!   Subroutine version of `Tril(A, diagNo)`. On return `lu` keeps the
!   entries of `A` with `j - i <= diagNo` and every other entry of
!   `lu` is zero.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   The argument order is `(A, diagNo, lu)`, so use keywords when
!   `diagNo` is omitted, e.g. `CALL GetTril(A=A, lu=lu)`.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   lu : same type and kind as `A`, INTENT(OUT),
!     `SHAPE(lu) = SHAPE(A)`
!
! Formula:
!   $$lu_{ij} = \begin{cases}
!     a_{ij}, & j - i \le k \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: GetTril
! USE TriagUtility, ONLY: Tril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), lu(3, 3), i, j
! REAL(DFP) :: b(3, 5), lu2(3, 5)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! entries outside the triangle are set to zero
! lu = 99_I4B
! CALL GetTril(A=a, lu=lu)
! CALL OK(ALL(lu .EQ. Tril(a)), "integer, default diagNo")
!
! CALL GetTril(A=a, diagNo=1_I4B, lu=lu)
! CALL OK(ALL(lu .EQ. RESHAPE([11, 21, 31, 12, 22, 32, 0, 23, 33], &
!                             [3, 3])), "integer, diagNo=1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! CALL GetTril(A=b, diagNo=-1_I4B, lu=lu2)
! CALL OK(ALL(ABS(lu2 - Tril(b, diagNo=-1_I4B)) .LT. tol), &
!         "real (3,5), diagNo=-1")
!
! END PROGRAM main
!```

INTERFACE GetTril
  MODULE PURE SUBROUTINE GetTril_1(A, diagNo, lu)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_1

  MODULE PURE SUBROUTINE GetTril_2(A, diagNo, lu)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_2

  MODULE PURE SUBROUTINE GetTril_3(A, diagNo, lu)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_3

  MODULE PURE SUBROUTINE GetTril_4(A, diagNo, lu)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_4

  MODULE PURE SUBROUTINE GetTril_5(A, diagNo, lu)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_5

  MODULE PURE SUBROUTINE GetTril_6(A, diagNo, lu)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(OUT) :: lu(SIZE(A, 1), SIZE(A, 2))
  END SUBROUTINE GetTril_6
END INTERFACE GetTril

!----------------------------------------------------------------------------
!                                                                     GetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Gets the lower triangular part of a matrix as a vector
!
! Description:
!   Subroutine version of `Tril(A, flate, diagNo)`. `lu` is allocated
!   to the number of entries with `j - i <= diagNo` and filled in the
!   order of `TrilIndx(A, diagNo)`.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   `flate` only selects this version; its value is not used.
!
! Arguments:
!   A : REAL or INTEGER, rank(2) - input matrix
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   lu : same type and kind as `A`, ALLOCATABLE, INTENT(OUT), rank(1),
!     `SIZE(lu) = SIZE(TrilIndx(A, diagNo), 1)`
!
! Formula:
!   $$lu_p = a_{i_p j_p}, \quad (i_p, j_p) = TrilIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: GetTril
! USE TriagUtility, ONLY: Tril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), i, j
! INTEGER(I4B), ALLOCATABLE :: v(:)
! REAL(DFP) :: b(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! lu is reallocated to the number of entries
! ALLOCATE (v(2))
! CALL GetTril(A=a, flate=.TRUE., lu=v)
! CALL OK(SIZE(v) .EQ. 6_I4B, "integer, diagNo=0: size")
! CALL OK(ALL(v .EQ. [11, 22, 33, 21, 32, 31]), "integer, diagNo=0")
!
! CALL GetTril(A=a, flate=.TRUE., diagNo=-1_I4B, lu=v)
! CALL OK(ALL(v .EQ. [21, 32, 31]), "integer, diagNo=-1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! same result as Tril with flate
! CALL GetTril(A=b, flate=.TRUE., diagNo=1_I4B, lu=w)
! CALL OK(SIZE(w) .EQ. 9_I4B, "real (3,5), diagNo=1: size")
! CALL OK(ALL(ABS(w - Tril(b, flate=.TRUE., diagNo=1_I4B)) .LT. tol), &
!         "real (3,5), same as Tril")
!
! END PROGRAM main
!```

INTERFACE GetTril
  MODULE PURE SUBROUTINE GetTril_7(A, flate, diagNo, lu)
    REAL(REAL32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_7
  MODULE PURE SUBROUTINE GetTril_8(A, flate, diagNo, lu)
    REAL(REAL64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_8
  MODULE PURE SUBROUTINE GetTril_9(A, flate, diagNo, lu)
    INTEGER(INT8), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_9
  MODULE PURE SUBROUTINE GetTril_10(A, flate, diagNo, lu)
    INTEGER(INT16), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_10
  MODULE PURE SUBROUTINE GetTril_11(A, flate, diagNo, lu)
    INTEGER(INT32), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_11
  MODULE PURE SUBROUTINE GetTril_12(A, flate, diagNo, lu)
    INTEGER(INT64), INTENT(IN) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), ALLOCATABLE, INTENT(OUT) :: lu(:)
  END SUBROUTINE GetTril_12
END INTERFACE GetTril

!----------------------------------------------------------------------------
!                                                                     SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the upper triangular part of a matrix from a matrix
!
! Description:
!   Copies `lu(i, j)` into `A(i, j)` for every entry with
!   `j - i >= diagNo`. The other entries of `A` are not changed.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - Only the entries of `lu` at the triangle positions of `A` are read.
!     If `lu` is smaller than `A`, the triangle entries outside `lu`
!     are not changed.
!
! Arguments:
!   A : REAL or INTEGER, rank(2), INTENT(INOUT) - matrix to modify
!   lu : same type and kind as `A`, rank(2) - source matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is replaced by the triangle of `lu`
!
! Formula:
!   $$a_{ij} \leftarrow lu_{ij}, \quad j - i \ge k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTriu
! USE TriagUtility, ONLY: Triu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), c(3, 3), i, j
! REAL(DFP) :: b(3, 5), d(3, 5)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! entries outside the triangle keep their value
! c = -1_I4B
! CALL SetTriu(A=c, lu=a)
! CALL OK(ALL(c .EQ. RESHAPE([11, -1, -1, 12, 22, -1, 13, 23, 33], &
!                            [3, 3])), "integer, default diagNo")
!
! c = -1_I4B
! CALL SetTriu(A=c, lu=a, diagNo=-1_I4B)
! CALL OK(ALL(c .EQ. RESHAPE([11, 21, -1, 12, 22, 32, 13, 23, 33], &
!                            [3, 3])), "integer, diagNo=-1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! setting into a zero matrix gives Triu
! d = 0.0_DFP
! CALL SetTriu(A=d, lu=b, diagNo=1_I4B)
! CALL OK(ALL(ABS(d - Triu(b, diagNo=1_I4B)) .LT. tol), &
!         "real (3,5), diagNo=1")
!
! END PROGRAM main
!```

INTERFACE SetTriu
  MODULE PURE SUBROUTINE SetTriu_1(A, lu, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_1

  MODULE PURE SUBROUTINE SetTriu_2(A, lu, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_2

  MODULE PURE SUBROUTINE SetTriu_3(A, lu, diagNo)
    INTEGER(INT8), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_3

  MODULE PURE SUBROUTINE SetTriu_4(A, lu, diagNo)
    INTEGER(INT16), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_4

  MODULE PURE SUBROUTINE SetTriu_5(A, lu, diagNo)
    INTEGER(INT32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_5

  MODULE PURE SUBROUTINE SetTriu_6(A, lu, diagNo)
    INTEGER(INT64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTriu_6
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                     SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the upper triangular part of a matrix from a vector
!
! Description:
!   Writes `lu(p)` into `A(i_p, j_p)`, where `(i_p, j_p)` is row `p` of
!   `TriuIndx(A, diagNo)`. It is the inverse of `Triu(A, flate, diagNo)`.
!   The other entries of `A` are not changed.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - `flate` only selects this version; its value is not used.
!   - `SIZE(lu)` should be the number of entries in the triangle. If
!     `lu` is shorter, only the first `SIZE(lu)` triangle entries are
!     set; extra entries of `lu` are ignored.
!
! Arguments:
!   A : REAL or INTEGER, rank(2), INTENT(INOUT) - matrix to modify
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   lu : same type and kind as `A`, rank(1) - values of the triangle
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is replaced by the entries of `lu`
!
! Formula:
!   $$a_{i_p j_p} \leftarrow lu_p, \quad
!     (i_p, j_p) = TriuIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTriu
! USE TriagUtility, ONLY: Triu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: c(3, 3), v(6), i, j
! REAL(DFP) :: b(3, 5), d(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! lu is used in the same order as Triu with flate
! v = [1, 2, 3, 4, 5, 6]
! c = 0_I4B
! CALL SetTriu(A=c, flate=.TRUE., lu=v)
! CALL OK(ALL(c .EQ. RESHAPE([1, 0, 0, 4, 2, 0, 6, 5, 3], &
!                            [3, 3])), "integer, diagNo=0")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! round trip: Triu with flate, then SetTriu with flate
! w = Triu(b, flate=.TRUE., diagNo=1_I4B)
! d = 0.0_DFP
! CALL SetTriu(A=d, flate=.TRUE., lu=w, diagNo=1_I4B)
! CALL OK(ALL(ABS(d - Triu(b, diagNo=1_I4B)) .LT. tol), &
!         "real (3,5), round trip, diagNo=1")
!
! END PROGRAM main
!```

INTERFACE SetTriu
  MODULE PURE SUBROUTINE SetTriu_7(A, flate, lu, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_7
  MODULE PURE SUBROUTINE SetTriu_8(A, flate, lu, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_8
  MODULE PURE SUBROUTINE SetTriu_9(A, flate, lu, diagNo)
    INTEGER(INT8), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_9
  MODULE PURE SUBROUTINE SetTriu_10(A, flate, lu, diagNo)
    INTEGER(INT16), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_10
  MODULE PURE SUBROUTINE SetTriu_11(A, flate, lu, diagNo)
    INTEGER(INT32), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_11
  MODULE PURE SUBROUTINE SetTriu_12(A, flate, lu, diagNo)
    INTEGER(INT64), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTriu_12
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                     SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the upper triangular part of a matrix to a scalar
!
! Description:
!   Sets `A(i, j) = val` for every entry with `j - i >= diagNo`. The
!   other entries of `A` are not changed.
!
!   - `A` can be REAL32 or REAL64. There is no version for an integer
!     matrix.
!   - `val` can be REAL32, REAL64 or INTEGER(I4B); it is converted to
!     the kind of `A`.
!
! Arguments:
!   A : REAL, rank(2), INTENT(INOUT) - matrix to modify
!   val : REAL32, REAL64 or INTEGER(I4B) - value of the triangle
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is set to `val`
!
! Formula:
!   $$a_{ij} \leftarrow \mathrm{val}, \quad j - i \ge k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTriu
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: c(3, 3), e(3, 3), d(3, 5)
! INTEGER(I4B) :: i, j
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! real value on the triangle, then an integer value further away
! c = 0.0_DFP
! CALL SetTriu(A=c, val=5.0_DFP)
! CALL SetTriu(A=c, val=2_I4B, diagNo=1_I4B)
!
! DO j = 1, 3
!   DO i = 1, 3
!     e(i, j) = 0.0_DFP
!     IF (j - i .EQ. 0) e(i, j) = 5.0_DFP
!     IF (j - i .GE. 1) e(i, j) = 2.0_DFP
!   END DO
! END DO
! CALL OK(ALL(ABS(c - e) .LT. tol), "real (3,3), real and integer val")
!
! ! rectangular matrix
! d = 0.0_DFP
! CALL SetTriu(A=d, val=1.0_DFP, diagNo=-1_I4B)
! CALL OK(COUNT(d .GT. 0.5_DFP) .EQ. 14_I4B, "real (3,5), diagNo=-1")
!
! END PROGRAM main
!```

INTERFACE SetTriu
  MODULE PURE SUBROUTINE SetTriu_13(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: val
  END SUBROUTINE SetTriu_13

  MODULE PURE SUBROUTINE SetTriu_14(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: val
  END SUBROUTINE SetTriu_14

  MODULE PURE SUBROUTINE SetTriu_15(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), INTENT(IN) :: val
  END SUBROUTINE SetTriu_15

  MODULE PURE SUBROUTINE SetTriu_16(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: val
  END SUBROUTINE SetTriu_16

  MODULE PURE SUBROUTINE SetTriu_17(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: val
  END SUBROUTINE SetTriu_17

  MODULE PURE SUBROUTINE SetTriu_18(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), INTENT(IN) :: val
  END SUBROUTINE SetTriu_18
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                     SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the lower triangular part of a matrix from a matrix
!
! Description:
!   Copies `lu(i, j)` into `A(i, j)` for every entry with
!   `j - i <= diagNo`. The other entries of `A` are not changed.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - Only the entries of `lu` at the triangle positions of `A` are read.
!     If `lu` is smaller than `A`, the triangle entries outside `lu`
!     are not changed.
!
! Arguments:
!   A : REAL or INTEGER, rank(2), INTENT(INOUT) - matrix to modify
!   lu : same type and kind as `A`, rank(2) - source matrix
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is replaced by the triangle of `lu`
!
! Formula:
!   $$a_{ij} \leftarrow lu_{ij}, \quad j - i \le k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTril
! USE TriagUtility, ONLY: Tril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3, 3), c(3, 3), i, j
! REAL(DFP) :: b(3, 5), d(3, 5)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! DO j = 1, 3
!   DO i = 1, 3
!     a(i, j) = 10 * i + j
!   END DO
! END DO
!
! ! entries outside the triangle keep their value
! c = -1_I4B
! CALL SetTril(A=c, lu=a)
! CALL OK(ALL(c .EQ. RESHAPE([11, 21, 31, -1, 22, 32, -1, -1, 33], &
!                            [3, 3])), "integer, default diagNo")
!
! c = -1_I4B
! CALL SetTril(A=c, lu=a, diagNo=1_I4B)
! CALL OK(ALL(c .EQ. RESHAPE([11, 21, 31, 12, 22, 32, -1, 23, 33], &
!                            [3, 3])), "integer, diagNo=1")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! setting into a zero matrix gives Tril
! d = 0.0_DFP
! CALL SetTril(A=d, lu=b, diagNo=-1_I4B)
! CALL OK(ALL(ABS(d - Tril(b, diagNo=-1_I4B)) .LT. tol), &
!         "real (3,5), diagNo=-1")
!
! END PROGRAM main
!```

INTERFACE SetTril
  MODULE PURE SUBROUTINE SetTril_1(A, lu, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_1

  MODULE PURE SUBROUTINE SetTril_2(A, lu, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_2

  MODULE PURE SUBROUTINE SetTril_3(A, lu, diagNo)
    INTEGER(INT8), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_3

  MODULE PURE SUBROUTINE SetTril_4(A, lu, diagNo)
    INTEGER(INT16), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_4

  MODULE PURE SUBROUTINE SetTril_5(A, lu, diagNo)
    INTEGER(INT32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_5

  MODULE PURE SUBROUTINE SetTril_6(A, lu, diagNo)
    INTEGER(INT64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(IN) :: lu(:, :)
  END SUBROUTINE SetTril_6
END INTERFACE SetTril

!----------------------------------------------------------------------------
!                                                                     SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the lower triangular part of a matrix from a vector
!
! Description:
!   Writes `lu(p)` into `A(i_p, j_p)`, where `(i_p, j_p)` is row `p` of
!   `TrilIndx(A, diagNo)`. It is the inverse of `Tril(A, flate, diagNo)`.
!   The other entries of `A` are not changed.
!   It is available for REAL32, REAL64, INT8, INT16, INT32 and INT64.
!
!   - `flate` only selects this version; its value is not used.
!   - `SIZE(lu)` should be the number of entries in the triangle. If
!     `lu` is shorter, only the first `SIZE(lu)` triangle entries are
!     set; extra entries of `lu` are ignored.
!
! Arguments:
!   A : REAL or INTEGER, rank(2), INTENT(INOUT) - matrix to modify
!   flate : LOGICAL(LGT) - selects the vector version (value not used)
!   lu : same type and kind as `A`, rank(1) - values of the triangle
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is replaced by the entries of `lu`
!
! Formula:
!   $$a_{i_p j_p} \leftarrow lu_p, \quad
!     (i_p, j_p) = TrilIndx(A, k)(p, :)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTril
! USE TriagUtility, ONLY: Tril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: c(3, 3), v(6), i, j
! REAL(DFP) :: b(3, 5), d(3, 5)
! REAL(DFP), ALLOCATABLE :: w(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! lu is used in the same order as Tril with flate
! v = [1, 2, 3, 4, 5, 6]
! c = 0_I4B
! CALL SetTril(A=c, flate=.TRUE., lu=v)
! CALL OK(ALL(c .EQ. RESHAPE([1, 4, 6, 0, 2, 5, 0, 0, 3], &
!                            [3, 3])), "integer, diagNo=0")
!
! DO j = 1, 5
!   DO i = 1, 3
!     b(i, j) = REAL(10 * i + j, DFP)
!   END DO
! END DO
!
! ! round trip: Tril with flate, then SetTril with flate
! w = Tril(b, flate=.TRUE., diagNo=-1_I4B)
! d = 0.0_DFP
! CALL SetTril(A=d, flate=.TRUE., lu=w, diagNo=-1_I4B)
! CALL OK(ALL(ABS(d - Tril(b, diagNo=-1_I4B)) .LT. tol), &
!         "real (3,5), round trip, diagNo=-1")
!
! END PROGRAM main
!```

INTERFACE SetTril
  MODULE PURE SUBROUTINE SetTril_7(A, flate, lu, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_7
  MODULE PURE SUBROUTINE SetTril_8(A, flate, lu, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_8
  MODULE PURE SUBROUTINE SetTril_9(A, flate, lu, diagNo)
    INTEGER(INT8), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT8), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_9
  MODULE PURE SUBROUTINE SetTril_10(A, flate, lu, diagNo)
    INTEGER(INT16), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT16), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_10
  MODULE PURE SUBROUTINE SetTril_11(A, flate, lu, diagNo)
    INTEGER(INT32), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT32), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_11
  MODULE PURE SUBROUTINE SetTril_12(A, flate, lu, diagNo)
    INTEGER(INT64), INTENT(INOUT) :: A(:, :)
    LOGICAL(LGT), INTENT(IN) :: flate
    !! This variable is only for creating unique interface
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(INT64), INTENT(IN) :: lu(:)
  END SUBROUTINE SetTril_12
END INTERFACE SetTril

!----------------------------------------------------------------------------
!                                                                     SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Sets the lower triangular part of a matrix to a scalar
!
! Description:
!   Sets `A(i, j) = val` for every entry with `j - i <= diagNo`. The
!   other entries of `A` are not changed.
!
!   - `A` can be REAL32 or REAL64. There is no version for an integer
!     matrix.
!   - `val` can be REAL32, REAL64 or INTEGER(I4B); it is converted to
!     the kind of `A`.
!
! Arguments:
!   A : REAL, rank(2), INTENT(INOUT) - matrix to modify
!   val : REAL32, REAL64 or INTEGER(I4B) - value of the triangle
!   diagNo : INTEGER(I4B), OPTIONAL - first diagonal of the triangle,
!     default 0; > 0 super-diagonal, < 0 sub-diagonal
!
! Returns:
!   A : the triangle of `A` is set to `val`
!
! Formula:
!   $$a_{ij} \leftarrow \mathrm{val}, \quad j - i \le k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriagUtility, ONLY: SetTril
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: c(3, 3), e(3, 3), d(3, 5)
! INTEGER(I4B) :: i, j
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! real value on the triangle, then an integer value further away
! c = 0.0_DFP
! CALL SetTril(A=c, val=5.0_DFP)
! CALL SetTril(A=c, val=2_I4B, diagNo=-1_I4B)
!
! DO j = 1, 3
!   DO i = 1, 3
!     e(i, j) = 0.0_DFP
!     IF (j - i .EQ. 0) e(i, j) = 5.0_DFP
!     IF (j - i .LE. -1) e(i, j) = 2.0_DFP
!   END DO
! END DO
! CALL OK(ALL(ABS(c - e) .LT. tol), "real (3,3), real and integer val")
!
! ! rectangular matrix
! d = 0.0_DFP
! CALL SetTril(A=d, val=1.0_DFP, diagNo=1_I4B)
! CALL OK(COUNT(d .GT. 0.5_DFP) .EQ. 9_I4B, "real (3,5), diagNo=1")
!
! END PROGRAM main
!```

INTERFACE SetTril
  MODULE PURE SUBROUTINE SetTril_13(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: val
  END SUBROUTINE SetTril_13

  MODULE PURE SUBROUTINE SetTril_14(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: val
  END SUBROUTINE SetTril_14

  MODULE PURE SUBROUTINE SetTril_15(A, val, diagNo)
    REAL(REAL32), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), INTENT(IN) :: val
  END SUBROUTINE SetTril_15

  MODULE PURE SUBROUTINE SetTril_16(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL32), INTENT(IN) :: val
  END SUBROUTINE SetTril_16

  MODULE PURE SUBROUTINE SetTril_17(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    REAL(REAL64), INTENT(IN) :: val
  END SUBROUTINE SetTril_17

  MODULE PURE SUBROUTINE SetTril_18(A, val, diagNo)
    REAL(REAL64), INTENT(INOUT) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), INTENT(IN) :: val
  END SUBROUTINE SetTril_18
END INTERFACE SetTril

END MODULE TriagUtility
