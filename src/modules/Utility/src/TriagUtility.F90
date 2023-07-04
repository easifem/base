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
! summary:         Module for getting triangular parts of matrix
!
!# Introduction
!
! This module provides methods for getting and setting
! lower and upper triangular part of the matrix.
!
! This module is inspired by the same functionality avaiable in
! numpy
!
! Following methods are implemented or planned
!
! - TriuIndx()
! - TrilIndx()
! - Triu()
! - Tril()
! - GetTriu()
! - GetTril()
! - SetTriu()
! - SetTril()
!
!## TriuIndx
!
!```fortran
! TriuIndx(m=3, n=5, k = 0) !! matrix of shape (3,5) , diag = 0
! TriuIndx(m=3, n=5, k = 1) !! matrix of shape (3,5) , diag = 1
! TriuIndx(m=3, n=5, k = -1) !! matrix of shape (3,5) , diag = -1
! TriuIndx(m=4,  k = 0) !! square matrix of shape (4,4), diag = 0
! TriuIndx(A, k=0) !! A is a matrix (square or rectangle)
!```
!## TrilIndx
!
!```fortran
! TrilIndx(m=3, n=5, k = 0) !! matrix of shape (3,5) , diag = 0
! TrilIndx(m=4,  k = 0) !! square matrix of shape (4,4), diag = 0
! TrilIndx(A, k=0) !! A is a matrix (square or rectangle)
!```

MODULE TriagUtility
USE GlobalData, ONLY: DFP, REAL32, REAL64, I4B, INT8, INT16, INT32, INT64, &
  & LGT
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the indices of upper triangle in an int vector
!
!# Introduction
!
! This function returns the indices of upper triangle in a integer vec
! starting from diag number k.
!
! k denotes the diag number
! k = 0 => main diag
! k > 0 => super diag
! k < 0 => sub diag

INTERFACE
  MODULE PURE FUNCTION TriuIndx_1(m, n, diagNo) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    !! number of rows
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
    !! number of columns, default = m
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION TriuIndx_1
END INTERFACE

INTERFACE TriuIndx
  MODULE PROCEDURE TriuIndx_1
END INTERFACE TriuIndx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the indices of upper triangle in an int vector
!
!# Introduction
!
! This function returns the indices of upper triangle in a integer vec
! starting from diag number k. Please read at TriuIndx_1
!

INTERFACE
  MODULE PURE FUNCTION TriuIndx_2(A, diagNo) RESULT(ans)
    CLASS(*), INTENT(IN) :: A(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: diagNo
    !! diagonal number, default = 0
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION TriuIndx_2
END INTERFACE

INTERFACE TriuIndx
  MODULE PROCEDURE TriuIndx_2
END INTERFACE TriuIndx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the indices of lower triangle part of a matrix

INTERFACE
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
END INTERFACE

INTERFACE TrilIndx
  MODULE PROCEDURE TrilIndx_1
END INTERFACE TrilIndx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the indices of lower triangle part of a matrix

INTERFACE
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
END INTERFACE

INTERFACE TrilIndx
  MODULE PROCEDURE TrilIndx_2
END INTERFACE TrilIndx

!----------------------------------------------------------------------------
!                                                                      Triu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangle part of a matrix.

INTERFACE
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
END INTERFACE

INTERFACE Triu
  MODULE PROCEDURE Triu_1, Triu_2, Triu_3, Triu_4, Triu_5, Triu_6
END INTERFACE Triu

!----------------------------------------------------------------------------
!                                                                      Triu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  lower triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE Triu
  MODULE PROCEDURE Triu_7, Triu_8, Triu_9, Triu_10, Triu_11, Triu_12
END INTERFACE Triu

!----------------------------------------------------------------------------
!                                                                      Tril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the lower triangle part of a matrix

INTERFACE
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
END INTERFACE

INTERFACE Tril
  MODULE PROCEDURE Tril_1, Tril_2, Tril_3, Tril_4, Tril_5, Tril_6
END INTERFACE Tril

!----------------------------------------------------------------------------
!                                                                      Tril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the lower triangle part of a matrix

INTERFACE
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
END INTERFACE

INTERFACE Tril
  MODULE PROCEDURE Tril_7, Tril_8, Tril_9, Tril_10, Tril_11, Tril_12
END INTERFACE Tril

!----------------------------------------------------------------------------
!                                                                   GetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangle part of a matrix

INTERFACE
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
END INTERFACE

INTERFACE GetTriu
  MODULE PROCEDURE GetTriu_1, GetTriu_2, GetTriu_3, GetTriu_4, &
  & GetTriu_5, GetTriu_6
END INTERFACE GetTriu

!----------------------------------------------------------------------------
!                                                                   GetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  upper triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE GetTriu
  MODULE PROCEDURE GetTriu_7, GetTriu_8, GetTriu_9, GetTriu_10, &
  & GetTriu_11, GetTriu_12
END INTERFACE GetTriu

!----------------------------------------------------------------------------
!                                                                   GetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  lower triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE GetTril
  MODULE PROCEDURE GetTril_1, GetTril_2, GetTril_3, GetTril_4, &
  & GetTril_5, GetTril_6
END INTERFACE GetTril

!----------------------------------------------------------------------------
!                                                                  GetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  lower triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE GetTril
  MODULE PROCEDURE GetTril_7, GetTril_8, GetTril_9, GetTril_10, &
  & GetTril_11, GetTril_12
END INTERFACE GetTril

!----------------------------------------------------------------------------
!                                                                   SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE SetTriu
  MODULE PROCEDURE SetTriu_1, SetTriu_2, SetTriu_3, SetTriu_4, &
  & SetTriu_5, SetTriu_6
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                   SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  upper triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE SetTriu
  MODULE PROCEDURE SetTriu_7, SetTriu_8, SetTriu_9, SetTriu_10, &
  & SetTriu_11, SetTriu_12
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                   SetTriu
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Set the upper triangle part to a scalar value

INTERFACE

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

END INTERFACE

INTERFACE SetTriu
  MODULE PROCEDURE SetTriu_13, SetTriu_14, SetTriu_15, &
      & SetTriu_16, SetTriu_17, SetTriu_18
END INTERFACE SetTriu

!----------------------------------------------------------------------------
!                                                                   SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the upper triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE SetTril
  MODULE PROCEDURE SetTril_1, SetTril_2, SetTril_3, SetTril_4, &
  & SetTril_5, SetTril_6
END INTERFACE SetTril

!----------------------------------------------------------------------------
!                                                                   SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Returns the  upper triangle part of an int vector

INTERFACE
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
END INTERFACE

INTERFACE SetTril
  MODULE PROCEDURE SetTril_7, SetTril_8, SetTril_9, SetTril_10, &
  & SetTril_11, SetTril_12
END INTERFACE SetTril

!----------------------------------------------------------------------------
!                                                                   SetTril
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Set lower triangle part to a scalar value

INTERFACE

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

END INTERFACE

INTERFACE SetTril
  MODULE PROCEDURE SetTril_13, SetTril_14, SetTril_15, &
      & SetTril_16, SetTril_17, SetTril_18
END INTERFACE SetTril

END MODULE TriagUtility
