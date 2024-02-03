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

MODULE AppendUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: ColConcat
PUBLIC :: OPERATOR(.ColConcat.)
PUBLIC :: RowConcat
PUBLIC :: OPERATOR(.RowConcat.)
PUBLIC :: Append
PUBLIC :: OPERATOR(.Append.)
PUBLIC :: Expand

!----------------------------------------------------------------------------
!                                                            Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Expand the vector
!
!# Introduction
! Expand the vector and add an element.
!
! reference
! https://github.com/jacobwilliams/fortran-csv-module/blob/master/src/
! csv_utilities.f90

INTERFACE Expand
  MODULE PURE SUBROUTINE expand_int8(vec, n, chunk_size, val, finished)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    INTEGER(INT8), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_int8

  MODULE PURE SUBROUTINE expand_int16(vec, n, chunk_size, val, finished)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    INTEGER(INT16), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_int16

  MODULE PURE SUBROUTINE expand_int32(vec, n, chunk_size, val, finished)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    INTEGER(INT32), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_int32

  MODULE PURE SUBROUTINE expand_int64(vec, n, chunk_size, val, finished)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_int64
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                      Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 July 2022
! summary: Expand the real vector

INTERFACE Expand
  MODULE PURE SUBROUTINE expand_real32(vec, n, chunk_size, val, finished)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    REAL(REAL32), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_real32

  MODULE PURE SUBROUTINE expand_real64(vec, n, chunk_size, val, finished)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vec(:)
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `vec`.
    !! must be initialized to `size(vec)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `vec` in blocks of this size (>0)
    REAL(REAL64), OPTIONAL, INTENT(IN) :: val
    !! the value to add to `vec`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `vec`
    !! as its correct size (`n`)
  END SUBROUTINE expand_real64
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                      Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Append a scalar or vector to vector
!
!# Introduction
!
!- Append a scalar integer to an integer vector
!- Append a integer vector and scalar to an integer vector
!- Append a scalar real to an real vector
!- Append a real vector to a real vector

INTERFACE Append
  ! Append a scalar int to a vector of int
  MODULE PURE SUBROUTINE Append_1a(A, ENTRY)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY
  END SUBROUTINE Append_1a

  ! Append a scalar real to a vector of real
  MODULE PURE SUBROUTINE Append_1b(A, ENTRY)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY
  END SUBROUTINE Append_1b

  ! Append a scalar and vector int to a vector of int
  MODULE PURE SUBROUTINE Append_1c(C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B
  END SUBROUTINE Append_1c

  ! Append a scalar and vector real to a vector of real
  MODULE PURE SUBROUTINE Append_1d(C, A, B)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B
  END SUBROUTINE Append_1d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Append two vectors of real or int
!
!# Introduction
!
!- Append a vector of int to another vector of int
!- Append a vector of real to another vector of real
!- Append two vectors of int to another vector of int
!- Append two vector of real to another vector of real

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2a(A, ENTRY)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
  END SUBROUTINE Append_2a

  MODULE PURE SUBROUTINE Append_2b(A, ENTRY)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
  END SUBROUTINE Append_2b

  MODULE PURE SUBROUTINE Append_2c(C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
  END SUBROUTINE Append_2c

  MODULE PURE SUBROUTINE Append_2d(C, A, B)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
  END SUBROUTINE Append_2d

  MODULE PURE SUBROUTINE Append_2e(D, C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: D(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
    INTEGER(I4B), INTENT(IN) :: C(:)
  END SUBROUTINE Append_2e

  MODULE PURE SUBROUTINE Append_2f(D, C, A, B)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: D(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
    REAL(DFP), INTENT(IN) :: C(:)
  END SUBROUTINE Append_2f

END INTERFACE Append

!----------------------------------------------------------------------------
!                                                      Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Masked append
!
!# Introduction
!
!- Append a scalar integer to a vector of int
!- Append a scalar real to a vector of reals
!- Append a scalar and vector of int to another vector of int
!- Append a scalar and vector real to another vector of real

INTERFACE Append
  MODULE PURE SUBROUTINE Append_3a(A, ENTRY, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3a

  MODULE PURE SUBROUTINE Append_3b(A, ENTRY, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3b

  MODULE PURE SUBROUTINE Append_3c(C, A, B, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3c

  MODULE PURE SUBROUTINE Append_3d(C, A, B, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                      Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Masked append two vectors to another vector of int/real
!
!# Introduction
!
!- Append a vector of int to another vector of int
!- Append a vector of real to another vector of real

INTERFACE Append
  MODULE PURE SUBROUTINE Append_4a(A, ENTRY, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4a

  MODULE PURE SUBROUTINE Append_4b(A, ENTRY, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4b

  MODULE PURE SUBROUTINE Append_4c(C, A, B, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4c

  MODULE PURE SUBROUTINE Append_4d(C, A, B, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                      Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Append a scalar INTEGER  to  INTEGER  vec tor

INTERFACE OPERATOR(.Append.)
  MODULE PURE FUNCTION func_Append_1a(A, ENTRY) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_1a

  MODULE PURE FUNCTION func_Append_1b(A, ENTRY) RESULT(ans)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Append two vectors of INTEGER

INTERFACE OPERATOR(.APPEND.)
  MODULE PURE FUNCTION func_Append_2a(A, ENTRY) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_2a

  MODULE PURE FUNCTION func_Append_2b(A, ENTRY) RESULT(ans)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_2b

END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat columns of two vectors

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(REAL32), INTENT(IN) :: b(:)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1a

  MODULE PURE FUNCTION colConcat_1b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(REAL64), INTENT(IN) :: b(:)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1b

  MODULE PURE FUNCTION colConcat_1c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1c

  MODULE PURE FUNCTION colConcat_1d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1d

  MODULE PURE FUNCTION colConcat_1e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1e

  MODULE PURE FUNCTION colConcat_1f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1f
END INTERFACE ColConcat

INTERFACE OPERATOR(.ColConcat.)
  MODULE PROCEDURE colConcat_1a, colConcat_1b, colConcat_1c, &
    & colConcat_1d, colConcat_1e, colConcat_1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat columns of a matrix and a vector

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_2a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:, :)
    REAL(REAL32), INTENT(IN) :: b(:)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2a

  MODULE PURE FUNCTION colConcat_2b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:, :)
    REAL(REAL64), INTENT(IN) :: b(:)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2b

  MODULE PURE FUNCTION colConcat_2c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:, :)
    INTEGER(INT64), INTENT(IN) :: b(:)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2c

  MODULE PURE FUNCTION colConcat_2d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:, :)
    INTEGER(INT32), INTENT(IN) :: b(:)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2d

  MODULE PURE FUNCTION colConcat_2e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:, :)
    INTEGER(INT16), INTENT(IN) :: b(:)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2e

  MODULE PURE FUNCTION colConcat_2f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:, :)
    INTEGER(INT8), INTENT(IN) :: b(:)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_2f
END INTERFACE ColConcat

INTERFACE OPERATOR(.ColConcat.)
  MODULE PROCEDURE colConcat_2a, colConcat_2b, colConcat_2c, &
    & colConcat_2d, colConcat_2e, colConcat_2f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat columns of rank1 and rank2 array

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_3a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(REAL32), INTENT(IN) :: b(:, :)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3a

  MODULE PURE FUNCTION colConcat_3b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(REAL64), INTENT(IN) :: b(:, :)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3b

  MODULE PURE FUNCTION colConcat_3c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:, :)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3c

  MODULE PURE FUNCTION colConcat_3d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:, :)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3d

  MODULE PURE FUNCTION colConcat_3e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:, :)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3e

  MODULE PURE FUNCTION colConcat_3f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:, :)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_3f
END INTERFACE ColConcat

INTERFACE OPERATOR(.ColConcat.)
  MODULE PROCEDURE colConcat_3a, colConcat_3b, colConcat_3c, &
    & colConcat_3d, colConcat_3e, colConcat_3f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat columns of rank2 and rank2 array

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_4a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:, :)
    REAL(REAL32), INTENT(IN) :: b(:, :)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4a

  MODULE PURE FUNCTION colConcat_4b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:, :)
    REAL(REAL64), INTENT(IN) :: b(:, :)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4b

  MODULE PURE FUNCTION colConcat_4c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:, :)
    INTEGER(INT64), INTENT(IN) :: b(:, :)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4c

  MODULE PURE FUNCTION colConcat_4d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:, :)
    INTEGER(INT32), INTENT(IN) :: b(:, :)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4d

  MODULE PURE FUNCTION colConcat_4e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:, :)
    INTEGER(INT16), INTENT(IN) :: b(:, :)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4e

  MODULE PURE FUNCTION colConcat_4f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:, :)
    INTEGER(INT8), INTENT(IN) :: b(:, :)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_4f
END INTERFACE ColConcat

INTERFACE OPERATOR(.ColConcat.)
  MODULE PROCEDURE colConcat_4a, colConcat_4b, colConcat_4c, &
    & colConcat_4d, colConcat_4e, colConcat_4f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat rows of two vectors

INTERFACE RowConcat
  MODULE PURE FUNCTION rowConcat_1a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(REAL32), INTENT(IN) :: b(:)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1a

  MODULE PURE FUNCTION rowConcat_1b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(REAL64), INTENT(IN) :: b(:)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1b

  MODULE PURE FUNCTION rowConcat_1c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1c

  MODULE PURE FUNCTION rowConcat_1d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1d

  MODULE PURE FUNCTION rowConcat_1e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1e

  MODULE PURE FUNCTION rowConcat_1f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_1f
END INTERFACE RowConcat

INTERFACE OPERATOR(.rowConcat.)
  MODULE PROCEDURE rowConcat_1a, rowConcat_1b, rowConcat_1c, &
    & rowConcat_1d, rowConcat_1e, rowConcat_1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat rows of a matrix and a vector

INTERFACE RowConcat
  MODULE PURE FUNCTION rowConcat_2a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:, :)
    REAL(REAL32), INTENT(IN) :: b(:)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2a

  MODULE PURE FUNCTION rowConcat_2b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:, :)
    REAL(REAL64), INTENT(IN) :: b(:)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2b

  MODULE PURE FUNCTION rowConcat_2c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:, :)
    INTEGER(INT64), INTENT(IN) :: b(:)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2c

  MODULE PURE FUNCTION rowConcat_2d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:, :)
    INTEGER(INT32), INTENT(IN) :: b(:)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2d

  MODULE PURE FUNCTION rowConcat_2e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:, :)
    INTEGER(INT16), INTENT(IN) :: b(:)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2e

  MODULE PURE FUNCTION rowConcat_2f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:, :)
    INTEGER(INT8), INTENT(IN) :: b(:)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_2f
END INTERFACE RowConcat

INTERFACE OPERATOR(.rowConcat.)
  MODULE PROCEDURE rowConcat_2a, rowConcat_2b, rowConcat_2c, &
    & rowConcat_2d, rowConcat_2e, rowConcat_2f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat rows of rank1 and rank2 array

INTERFACE RowConcat
  MODULE PURE FUNCTION rowConcat_3a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(REAL32), INTENT(IN) :: b(:, :)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3a

  MODULE PURE FUNCTION rowConcat_3b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(REAL64), INTENT(IN) :: b(:, :)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3b

  MODULE PURE FUNCTION rowConcat_3c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:, :)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3c

  MODULE PURE FUNCTION rowConcat_3d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:, :)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3d

  MODULE PURE FUNCTION rowConcat_3e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:, :)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3e

  MODULE PURE FUNCTION rowConcat_3f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:, :)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_3f
END INTERFACE RowConcat

INTERFACE OPERATOR(.rowConcat.)
  MODULE PROCEDURE rowConcat_3a, rowConcat_3b, rowConcat_3c, &
    & rowConcat_3d, rowConcat_3e, rowConcat_3f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Concat rows of rank2 and rank2 array

INTERFACE RowConcat
  MODULE PURE FUNCTION rowConcat_4a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:, :)
    REAL(REAL32), INTENT(IN) :: b(:, :)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4a

  MODULE PURE FUNCTION rowConcat_4b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:, :)
    REAL(REAL64), INTENT(IN) :: b(:, :)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4b

  MODULE PURE FUNCTION rowConcat_4c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:, :)
    INTEGER(INT64), INTENT(IN) :: b(:, :)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4c

  MODULE PURE FUNCTION rowConcat_4d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:, :)
    INTEGER(INT32), INTENT(IN) :: b(:, :)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4d

  MODULE PURE FUNCTION rowConcat_4e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:, :)
    INTEGER(INT16), INTENT(IN) :: b(:, :)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4e

  MODULE PURE FUNCTION rowConcat_4f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:, :)
    INTEGER(INT8), INTENT(IN) :: b(:, :)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION rowConcat_4f
END INTERFACE RowConcat

INTERFACE OPERATOR(.rowConcat.)
  MODULE PROCEDURE rowConcat_4a, rowConcat_4b, rowConcat_4c, &
    & rowConcat_4d, rowConcat_4e, rowConcat_4f
END INTERFACE

END MODULE AppendUtility
