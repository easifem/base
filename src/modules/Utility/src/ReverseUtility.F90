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
! date: 2025-10-14
! summary: Reverses the entries of arrays in place.
!
! Description:
!   This module contains the generic subroutine `Reverse`, which
!   reverses the order of entries of an array in place.
!
!```fortran
! CALL Reverse(ans, n1, n2)
! CALL Reverse(ans, r1, r2, c1, c2, dim)
! CALL Reverse(ans, r1, r2, c1, c2, d1, d2, dim)
!```
!
!   Only the given range (or block) is changed. `Reverse` is available
!   for
!
!   - integer and real vectors (`ans(n1:n2)`),
!   - integer and real matrices (`ans(r1:r2, c1:c2)` along `dim`),
!   - `REAL32` and `REAL64` rank-3 arrays (`ans(r1:r2, c1:c2, d1:d2)`
!     along `dim`).

MODULE ReverseUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE

PRIVATE

PUBLIC :: Reverse

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Reverses a part of an integer vector in place.
!
! Description:
!   Reverses the order of the entries `ans(n1:n2)` in place. The
!   entries outside `n1:n2` are not changed. It is available for
!   `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - If `n1 >= n2`, nothing is changed.
!   - `n1` and `n2` are not checked against the bounds of `ans`.
!
! Arguments:
!   ans : INTEGER, rank(1) - vector, modified in place
!   n1 : INTEGER(I4B) - first index of the range, `n1 >= 1`
!   n2 : INTEGER(I4B) - last index of the range, `n2 <= SIZE(ans)`
!
! Returns:
!   ans : INTEGER, rank(1) - `ans(n1:n2)` in reverse order
!
! Formula:
!   $$\mathrm{ans}_{n_1+k} \leftarrow \mathrm{ans}_{n_2-k}, \quad
!     k = 0, \dots, n_2 - n_1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReverseUtility, ONLY: Reverse
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(6)
!
! ! reverse the whole vector (even length)
! a = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
! CALL Reverse(a, 1_I4B, 6_I4B)
! CALL OK(ALL(a .EQ. [6_I4B, 5_I4B, 4_I4B, 3_I4B, 2_I4B, 1_I4B]), &
!         "reverse a(1:6)")
!
! ! reverse a part (odd length), other entries are untouched
! a = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
! CALL Reverse(a, 2_I4B, 4_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 4_I4B, 3_I4B, 2_I4B, 5_I4B, 6_I4B]), &
!         "reverse a(2:4)")
!
! ! reversing twice gives back the input
! CALL Reverse(a, 2_I4B, 4_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
!         "reverse twice")
!
! ! n1 = n2 and n1 > n2 do nothing
! CALL Reverse(a, 3_I4B, 3_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
!         "n1 = n2")
! CALL Reverse(a, 5_I4B, 2_I4B)
! CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
!         "n1 > n2")
!
! END PROGRAM main
!```

INTERFACE Reverse
  MODULE PURE SUBROUTINE Reverse_Int8_R1(ans, n1, n2)
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Int8_R1

  MODULE PURE SUBROUTINE Reverse_Int16_R1(ans, n1, n2)
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Int16_R1

  MODULE PURE SUBROUTINE Reverse_Int32_R1(ans, n1, n2)
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Int32_R1

  MODULE PURE SUBROUTINE Reverse_Int64_R1(ans, n1, n2)
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Int64_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Reverses a part of a real vector in place.
!
! Description:
!   Reverses the order of the entries `ans(n1:n2)` in place. The
!   entries outside `n1:n2` are not changed. It is available for
!   `REAL32` and `REAL64`.
!
!   - If `n1 >= n2`, nothing is changed.
!   - `n1` and `n2` are not checked against the bounds of `ans`.
!
! Arguments:
!   ans : REAL, rank(1) - vector, modified in place
!   n1 : INTEGER(I4B) - first index of the range, `n1 >= 1`
!   n2 : INTEGER(I4B) - last index of the range, `n2 <= SIZE(ans)`
!
! Returns:
!   ans : REAL, rank(1) - `ans(n1:n2)` in reverse order
!
! Formula:
!   $$\mathrm{ans}_{n_1+k} \leftarrow \mathrm{ans}_{n_2-k}, \quad
!     k = 0, \dots, n_2 - n_1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReverseUtility, ONLY: Reverse
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(5), b(5)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! reverse the whole vector (odd length)
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
! CALL Reverse(a, 1_I4B, 5_I4B)
! b = [5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
! CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(1:5)")
!
! ! reverse the last four entries (even length)
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
! CALL Reverse(a, 2_I4B, 5_I4B)
! b = [1.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP]
! CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(2:5)")
! CALL OK(ABS(a(1) - 1.0_DFP) .LT. tol, "a(1) is untouched")
!
! ! reverse two entries
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
! CALL Reverse(a, 1_I4B, 2_I4B)
! b = [2.0_DFP, 1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
! CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(1:2)")
!
! END PROGRAM main
!```

INTERFACE Reverse
  MODULE PURE SUBROUTINE Reverse_Real32_R1(ans, n1, n2)
    REAL(REAL32), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Real32_R1

  MODULE PURE SUBROUTINE Reverse_Real64_R1(ans, n1, n2)
    REAL(REAL64), INTENT(INOUT) :: ans(:)
    !! vector, ans(n1:n2) is reversed in place
    INTEGER(I4B), INTENT(IN) :: n1, n2
    !! range of entries to reverse
  END SUBROUTINE Reverse_Real64_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Reverses a block of an integer matrix along a dimension.
!
! Description:
!   Works on the block `ans(r1:r2, c1:c2)` in place. The entries
!   outside the block are not changed. It is available for
!   `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `dim=1` reverses the order of the rows `r1:r2` in each column
!     `c1:c2` (flips the block upside down).
!   - `dim=2` reverses the order of the columns `c1:c2` in each row
!     `r1:r2` (flips the block left to right).
!   - Any other value of `dim` changes nothing.
!   - The bounds are not checked against the shape of `ans`.
!
! Arguments:
!   ans : INTEGER, rank(2) - matrix, modified in place
!   r1, r2 : INTEGER(I4B) - row range of the block
!   c1, c2 : INTEGER(I4B) - column range of the block
!   dim : INTEGER(I4B) - dimension to reverse, 1 or 2
!
! Returns:
!   ans : INTEGER, rank(2) - the block reversed along `dim`
!
! Formula:
!   $$\mathrm{dim}=1: \ \mathrm{ans}_{r_1+k,\,j} \leftarrow
!     \mathrm{ans}_{r_2-k,\,j}, \quad c_1 \le j \le c_2$$
!   $$\mathrm{dim}=2: \ \mathrm{ans}_{i,\,c_1+k} \leftarrow
!     \mathrm{ans}_{i,\,c_2-k}, \quad r_1 \le i \le r_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ReverseUtility, ONLY: Reverse
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(4, 4), a0(4, 4), b(4, 4)
! INTEGER(I4B) :: ii
!
! a0 = RESHAPE([(ii, ii=1, 16)], [4, 4])
!
! ! dim=1: reverse the order of rows 2:4 in columns 2:3
! a = a0
! CALL Reverse(a, 2_I4B, 4_I4B, 2_I4B, 3_I4B, 1_I4B)
! b = a0
! b(2:4, 2:3) = a0(4:2:-1, 2:3)
! CALL OK(ALL(a .EQ. b), "dim=1 on a(2:4, 2:3)")
! CALL OK(a(1, 2) .EQ. a0(1, 2), "row 1 is untouched")
! CALL OK(ALL(a(:, 1) .EQ. a0(:, 1)), "column 1 is untouched")
!
! ! dim=2: reverse the order of columns 1:4 in rows 1:3
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 4_I4B, 2_I4B)
! b = a0
! b(1:3, :) = a0(1:3, 4:1:-1)
! CALL OK(ALL(a .EQ. b), "dim=2 on a(1:3, 1:4)")
! CALL OK(ALL(a(4, :) .EQ. a0(4, :)), "row 4 is untouched")
!
! ! an invalid dim does nothing
! a = a0
! CALL Reverse(a, 1_I4B, 4_I4B, 1_I4B, 4_I4B, 3_I4B)
! CALL OK(ALL(a .EQ. a0), "dim=3 does nothing for a matrix")
!
! END PROGRAM main
!```

INTERFACE Reverse
  MODULE PURE SUBROUTINE Reverse_Int8_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT8), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int8_R2

  MODULE PURE SUBROUTINE Reverse_Int16_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT16), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int16_R2

  MODULE PURE SUBROUTINE Reverse_Int32_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT32), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int32_R2

  MODULE PURE SUBROUTINE Reverse_Int64_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT64), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int64_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Reverses a block of a real matrix along a dimension.
!
! Description:
!   Works on the block `ans(r1:r2, c1:c2)` in place. The entries
!   outside the block are not changed. It is available for
!   `REAL32` and `REAL64`.
!
!   - `dim=1` reverses the order of the rows `r1:r2` in each column
!     `c1:c2` (flips the block upside down).
!   - `dim=2` reverses the order of the columns `c1:c2` in each row
!     `r1:r2` (flips the block left to right).
!   - Any other value of `dim` changes nothing.
!   - The bounds are not checked against the shape of `ans`.
!
! Arguments:
!   ans : REAL, rank(2) - matrix, modified in place
!   r1, r2 : INTEGER(I4B) - row range of the block
!   c1, c2 : INTEGER(I4B) - column range of the block
!   dim : INTEGER(I4B) - dimension to reverse, 1 or 2
!
! Returns:
!   ans : REAL, rank(2) - the block reversed along `dim`
!
! Formula:
!   $$\mathrm{dim}=1: \ \mathrm{ans}_{r_1+k,\,j} \leftarrow
!     \mathrm{ans}_{r_2-k,\,j}, \quad c_1 \le j \le c_2$$
!   $$\mathrm{dim}=2: \ \mathrm{ans}_{i,\,c_1+k} \leftarrow
!     \mathrm{ans}_{i,\,c_2-k}, \quad r_1 \le i \le r_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReverseUtility, ONLY: Reverse
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3, 3), a0(3, 3), b(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B) :: ii
!
! a0 = RESHAPE([(REAL(ii, DFP), ii=1, 9)], [3, 3])
!
! ! dim=1: flip the matrix upside down
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B)
! b = a0(3:1:-1, :)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=1 on the whole matrix")
!
! ! dim=2: flip the matrix left to right
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 2_I4B)
! b = a0(:, 3:1:-1)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2 on the whole matrix")
!
! ! dim=2 on a block, other entries are untouched
! a = a0
! CALL Reverse(a, 2_I4B, 3_I4B, 2_I4B, 3_I4B, 2_I4B)
! b = a0
! b(2:3, 2:3) = a0(2:3, 3:2:-1)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2 on a(2:3, 2:3)")
!
! END PROGRAM main
!```

INTERFACE Reverse
  MODULE PURE SUBROUTINE Reverse_Real32_R2(ans, r1, r2, c1, c2, dim)
    REAL(REAL32), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Real32_R2

  MODULE PURE SUBROUTINE Reverse_Real64_R2(ans, r1, r2, c1, c2, dim)
    REAL(REAL64), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Real64_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Reverses a block of a real rank-3 array along a dimension.
!
! Description:
!   Works on the block `ans(r1:r2, c1:c2, d1:d2)` in place. The
!   entries outside the block are not changed. It is available for
!   `REAL32` and `REAL64`.
!
!   - `dim=1`, `dim=2` or `dim=3` reverses the order of the entries
!     along the first, second or third index of the block.
!   - Any other value of `dim` changes nothing.
!   - The bounds are not checked against the shape of `ans`.
!
! Arguments:
!   ans : REAL, rank(3) - array, modified in place
!   r1, r2 : INTEGER(I4B) - range of the first index
!   c1, c2 : INTEGER(I4B) - range of the second index
!   d1, d2 : INTEGER(I4B) - range of the third index
!   dim : INTEGER(I4B) - dimension to reverse, 1, 2 or 3
!
! Returns:
!   ans : REAL, rank(3) - the block reversed along `dim`
!
! Formula:
!   For `dim=3`, and similarly for `dim=1` and `dim=2`:
!   $$\mathrm{ans}_{i,\,j,\,d_1+k} \leftarrow
!     \mathrm{ans}_{i,\,j,\,d_2-k}, \quad k = 0, \dots, d_2 - d_1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReverseUtility, ONLY: Reverse
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3, 3, 3), a0(3, 3, 3), b(3, 3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B) :: ii
!
! a0 = RESHAPE([(REAL(ii, DFP), ii=1, 27)], [3, 3, 3])
!
! ! dim=1 on the whole array
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B)
! b = a0(3:1:-1, :, :)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=1")
!
! ! dim=2 on the whole array
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 2_I4B)
! b = a0(:, 3:1:-1, :)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2")
!
! ! dim=3 on the whole array
! a = a0
! CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 3_I4B)
! b = a0(:, :, 3:1:-1)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=3")
!
! ! dim=3 on a block, other entries are untouched
! a = a0
! CALL Reverse(a, 1_I4B, 2_I4B, 2_I4B, 3_I4B, 1_I4B, 3_I4B, 3_I4B)
! b = a0
! b(1:2, 2:3, :) = a0(1:2, 2:3, 3:1:-1)
! CALL OK(ALL(ABS(a - b) .LT. tol), "dim=3 on a(1:2, 2:3, 1:3)")
!
! END PROGRAM main
!```

INTERFACE Reverse
  MODULE PURE SUBROUTINE Reverse_Real64_R3(ans, r1, r2, c1, c2, d1, d2, &
                                           dim)
    REAL(REAL64), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2, d1, d2
    !! Extent of ans(r1:r2, c1:c2, d1:d2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the dim1
    !! dim=2, reverse the dim2
    !! dim=3, reverse the dim3
  END SUBROUTINE Reverse_Real64_R3

  MODULE PURE SUBROUTINE Reverse_Real32_R3(ans, r1, r2, c1, c2, d1, d2, &
                                           dim)
    REAL(REAL32), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2, d1, d2
    !! Extent of ans(r1:r2, c1:c2, d1:d2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the dim1
    !! dim=2, reverse the dim2
    !! dim=3, reverse the dim3
  END SUBROUTINE Reverse_Real32_R3
END INTERFACE Reverse

END MODULE ReverseUtility
