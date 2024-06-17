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

MODULE DiagUtility
USE GlobalData, ONLY: I4B, INT8, INT16, INT32, INT64, &
& REAL32, REAL64, DFP
#ifdef USE_Int128
USE GlobaData, ONLY: Int128
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
! summary: Make a Diagonal matrix from int8 vector

INTERFACE
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
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_1, Diag_2, Diag_3, Diag_4, Diag_5, &
    & Diag_6
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                                      Diag
!----------------------------------------------------------------------------

#ifdef USE_Int128

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Make diagonal matrix from Int128 vector.

INTERFACE
  MODULE PURE FUNCTION Diag_7(a) RESULT(Ans)
    INTEGER(Int128), INTENT(IN) :: a(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(a))
  END FUNCTION Diag_7
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_7
END INTERFACE Diag

#endif

!----------------------------------------------------------------------------
!                                                                       Diag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Get the diagNo of matrix
!
!# Introduction
!
!- This routine returns the diagonal of matrix.
!- `diagNo=0` denotes main diagonal
!- `diagNo>0` denotes the super-diagonal
!- `diagNo<0` denotes the sub-diagonal
!- `d` is a one dimesional vector of default Reals (DFP)

INTERFACE
  MODULE PURE FUNCTION Diag_8(mat, diagNo) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
    !! diagonal  number
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! diagonal
  END FUNCTION Diag_8

END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_8
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                                   SetDiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Set the diagNo of matrix
!
!# Introduction
!
!- This routine sets the diagonal of matrix.
!
!- `diagNo=0` denotes main diagonal
!- `diagNo>0` denotes the super-diagonal
!- `diagNo<0` denotes the sub-diagonal
!
!- `d` is a one dimesional vector of (Int or float)
!- if `size(d)=1`, then all entries of the diagonal will be set to this
! value.
!- if `size(d) .ne. 1`, then the size of `d` should be atleast the size of
! diagonal number `diag`.

INTERFACE
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
END INTERFACE

INTERFACE SetDiag
  MODULE PROCEDURE SetDiag1, SetDiag2, SetDiag3, SetDiag4, &
    & SetDiag5, SetDiag6
END INTERFACE SetDiag

!----------------------------------------------------------------------------
!                                                               DiagSize
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION DiagSize1(n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
  !! size of matrix
    INTEGER(I4B), INTENT(IN) :: diagNo
  !! diagonal number
    INTEGER(I4B) :: ans
  !! size of diagonal
  END FUNCTION DiagSize1
END INTERFACE

INTERFACE DiagSize
  MODULE PROCEDURE DiagSize1
END INTERFACE DiagSize

!----------------------------------------------------------------------------
!                                                               DiagSize
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

INTERFACE DiagSize
  MODULE PROCEDURE DiagSize2
END INTERFACE DiagSize

!----------------------------------------------------------------------------
!                                                               DiagSize
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetTriDiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Set the diagNo of tri diagonal matrix
!
!# Introduction
!
!- This routine sets the diagonals of a tridiagonal matrix.
!
!- `d` denotes main diagonal
!- `da` denotes the super-diagonal
!- `db` denotes the sub-diagonal
!
!- `d, da, db` are one dimesional vectors of (Int or float)
!- if `size(d/da/db)=1`, then all entries of the diagonal will be set to this
! value.
!- if `size(d/da/db) .ne. 1`, then the size of `d/da/db` should be atleast
! the size of respective diagonals.

INTERFACE
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

END INTERFACE

INTERFACE SetTriDiag
  MODULE PROCEDURE SetTriDiag1, SetTriDiag2, SetTriDiag3, SetTriDiag4, &
    & SetTriDiag5, SetTriDiag6
END INTERFACE SetTriDiag

!----------------------------------------------------------------------------
!                                                                   Tridiag
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-11
! summary: Make a Tridiagonal matrix from main, sub, super diagonal

INTERFACE
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

END INTERFACE

INTERFACE Tridiag
  MODULE PROCEDURE Tridiag_1, Tridiag_2, Tridiag_3, Tridiag_4, Tridiag_5, &
    & Tridiag_6
END INTERFACE Tridiag

END MODULE DiagUtility
