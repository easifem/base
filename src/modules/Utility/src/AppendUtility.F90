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
! date: 2024-04-11
! summary:  This module contains method for appending to vectors and martrix
!
MODULE AppendUtility
USE GlobalData, ONLY: I4B, LGT, DFP
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
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
! summary: Expand the vector of INT8 if needed
!
! Expand the vector and add an element.
!
! reference:
! https://github.com/jacobwilliams/fortran-csv-module/blob/master/src/
! csv_utilities.f90
!
! Calling example:
!
! ```fortran
! CALL Expand(vec, n=0, chunk_size)
! CALL Expand(vec, n, chunk_size, finished=.TRUE.)
! ```
!
! Example
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP, I4B, LGT
! USE Display_Method, ONLY: Display
! USE AppendUtility, ONLY: Expand
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: aintvec(:)
! INTEGER(I4B) :: n, val
! ! First CALL
! ! We set n = 0, as aintvec is not allocated.
! val = 1
! n = 0
! CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.FALSE.)
! CALL Display(aintvec, "test1: aintvec = ", full=.TRUE.)
! CALL Display(n, "n = ")
! CALL Display(SIZE(aintvec), "test1: size of aintvec = ")
! val = 2
! CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.TRUE.)
! CALL Display(aintvec, "test2: aintvec= ", full=.TRUE.)
! CALL Display(n, "n = ")
! CALL Display(SIZE(aintvec), "test2: size of aintvec = ")
! END PROGRAM main
!```

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
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                            Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Expand the vector of INT16 if needed

INTERFACE Expand
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
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                            Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Expand the vector of INT32 if needed

INTERFACE Expand
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
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                            Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Expand the vector of INT64 if needed

INTERFACE Expand
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
!                                                             Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Expand the real vector (REAL32) if needed

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
END INTERFACE Expand

!----------------------------------------------------------------------------
!                                                             Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Expand the real vector (REAL64) if needed

INTERFACE Expand
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
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar INT or a vector of INT
!
! Append a scalar int to a vector of int
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! Append scalar to a vector
! CALL Append(avec, 1)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
!
! ! Append a vector to a vector
! CALL Append(avec, [2, 3])
! CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")
!
! DEALLOCATE (avec)
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_1a(A, ENTRY)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    !! On Return it will be [A, Entry]
    INTEGER(I4B), INTENT(IN) :: ENTRY
    !! Append Entry to A
  END SUBROUTINE Append_1a
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar (REAL) to a vector of real
!
! Append is a scalar real to a vector of real.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! Append scalar to a vector
! CALL Append(avec, 1)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
!
! ! Append a vector to a vector
! CALL Append(avec, [2, 3])
! CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")
!
! DEALLOCATE (avec)
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_1b(A, ENTRY)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    !! On return it will be [A, Entry]
    REAL(DFP), INTENT(IN) :: ENTRY
    !! Append Entry to A
  END SUBROUTINE Append_1b
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar or vector to vector
!
! Append a scalar INT and a vector of INT to a vector of INT.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! Append A and B vectors to C vector"
! ! Note that C is not allocated here, but this is not a problem.
! A = [1]
! B = [2]
! CALL Append(C, A, B)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(1):")
! ! A is vector B is scalar"
! ! Append A (Vector) and B (Scalar) to C vector"
! A = [1]
! CALL Append(C, A, 2)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(2):")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_1c(C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B
  END SUBROUTINE Append_1c
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar and vector of real to a vector of real
!
! Append a scalar and vector real to a vector of real
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! Append A and B vectors to C vector"
! ! Note that C is not allocated here, but this is not a problem.
! A = [1]
! B = [2]
! CALL Append(C, A, B)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(1):")
! ! A is vector B is scalar"
! ! Append A (Vector) and B (Scalar) to C vector"
! A = [1]
! CALL Append(C, A, 2)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(2):")
! END PROGRAM main
!```

INTERFACE Append
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
! date: 2026-10-16
! summary: Append a vector of INT to a vector of INT.
!
! Append a vector of INT to a vector of INT.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! Append scalar to a vector
! CALL Append(avec, 1)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
!
! ! Append a vector to a vector
! CALL Append(avec, [2, 3])
! CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")
!
! DEALLOCATE (avec)
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2a(A, ENTRY)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
  END SUBROUTINE Append_2a
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a vector of REAL to a vector of REAL.
!
! Append a vector of REAL to a vector of REAL.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! Append scalar to a vector
! CALL Append(avec, 1)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
!
! ! Append a vector to a vector
! CALL Append(avec, [2, 3])
! CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")
!
! DEALLOCATE (avec)
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2b(A, ENTRY)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
  END SUBROUTINE Append_2b
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append two vectors of INT to a vector of INT.
!
! Append two vectors of INT to a vector of INT.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! Append A and B vectors to C vector"
! ! Note that C is not allocated here, but this is not a problem.
! A = [1]
! B = [2]
! CALL Append(C, A, B)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(1):")
! ! A is vector B is scalar"
! ! Append A (Vector) and B (Scalar) to C vector"
! A = [1]
! CALL Append(C, A, 2)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(2):")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2c(C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
  END SUBROUTINE Append_2c
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append two vectors of REAL to a vector of REAL.
!
! Append two vectors of REAL to a vector of REAL.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! Append A and B vectors to C vector"
! ! Note that C is not allocated here, but this is not a problem.
! A = [1]
! B = [2]
! CALL Append(C, A, B)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(1):")
! ! A is vector B is scalar"
! ! Append A (Vector) and B (Scalar) to C vector"
! A = [1]
! CALL Append(C, A, 2)
! CALL OK(ALL(C .EQ. [1, 2]), "tests(2):")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2d(C, A, B)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
  END SUBROUTINE Append_2d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append three vectors of INT to a vector of INT.
!
! Append three vectors of INT to a vector of INT.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2e(D, C, A, B)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: D(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
    INTEGER(I4B), INTENT(IN) :: C(:)
  END SUBROUTINE Append_2e
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append three vectors of REAL to a vector of REAL.
!
! Append three vectors of REAL to a vector of REAL.
!
!@note
! While calling Append the initial vector can be unallocated in the begining.
!@endnote

INTERFACE Append
  MODULE PURE SUBROUTINE Append_2f(D, C, A, B)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: D(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
    REAL(DFP), INTENT(IN) :: C(:)
  END SUBROUTINE Append_2f
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: If mask is TRUE then append the scalar (INT) to a vector of INT.
!
! If mask is TRUE then append the scalar (INT) to a vector of INT.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: avec(:)
! ! Now lets us append a single ENTRY to a vector.
! CALL Append(avec, 1, mask=.TRUE.)
! CALL Append(avec, 1, mask=.FALSE.)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_3a(A, ENTRY, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3a
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: If mask is TRUE then append the scalar (REAL) to a vector of REAL.
!
! If mask is TRUE then append the scalar (REAL) to a vector of REAL.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: avec(:)
! ! Now lets us append a single ENTRY to a vector.
! CALL Append(avec, 1, mask=.TRUE.)
! CALL Append(avec, 1, mask=.FALSE.)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_3b(A, ENTRY, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3b
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Masked append
!
! If mask is TRUE then append a scalar (INT) and a vector of INT
! to a vector of INT.
!
!
! Example
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! A and B are vectors"
! A = [1, 2]
! B = [3, 4]
! ! Append B(1) and B(2) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .TRUE.])
! CALL OK(ALL(C .EQ. [1, 2, 3, 4]), "Append:")
! ! Append B(1) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append B to A, just assign A to C.
! CALL Append(C, A, B, mask=[.FALSE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! ! A is vector B is scalar
! ! Append 3 to A and then assign result to C
! CALL Append(C, A, 3, mask=.TRUE.)
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append 3 to A, assign result to C, that is C =A
! CALL Append(C, A, 3, mask=.FALSE.)
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_3c(C, A, B, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3c
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Masked append
!
! If mask is TRUE then append a scalar (REAL) and a vector of REAL
! to a vector of REAL.
!
!
! Example
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! A and B are vectors"
! A = [1, 2]
! B = [3, 4]
! ! Append B(1) and B(2) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .TRUE.])
! CALL OK(ALL(C .EQ. [1, 2, 3, 4]), "Append:")
! ! Append B(1) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append B to A, just assign A to C.
! CALL Append(C, A, B, mask=[.FALSE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! ! A is vector B is scalar
! ! Append 3 to A and then assign result to C
! CALL Append(C, A, 3, mask=.TRUE.)
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append 3 to A, assign result to C, that is C =A
! CALL Append(C, A, 3, mask=.FALSE.)
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_3d(C, A, B, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE Append_3d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-10-16
! summary: Masked append
!
! If mask(i) is true then append entry(i) to A.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: avec(:)
! ! Now lets us append a single ENTRY to a vector.
! CALL Append(avec, 1, mask=.TRUE.)
! CALL Append(avec, 1, mask=.FALSE.)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
! ! Appending a vector to a vector.
! CALL Append(avec, [2, 3], mask=[.TRUE., .TRUE.])
! CALL Append(avec, [4, 5], mask=[.TRUE., .FALSE.])
! CALL OK(ALL(avec .EQ. [1, 2, 3, 4]), "Append a vector")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_4a(A, ENTRY, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4a
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-10-16
! summary: Masked append
!
! If mask(i) is true then append entry(i) to A.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: avec(:)
! ! Now lets us append a single ENTRY to a vector.
! CALL Append(avec, 1, mask=.TRUE.)
! CALL Append(avec, 1, mask=.FALSE.)
! CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
! ! Appending a vector to a vector.
! CALL Append(avec, [2, 3], mask=[.TRUE., .TRUE.])
! CALL Append(avec, [4, 5], mask=[.TRUE., .FALSE.])
! CALL OK(ALL(avec .EQ. [1, 2, 3, 4]), "Append a vector")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_4b(A, ENTRY, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4b
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                            Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-10-16
! summary: Masked append
!
! If mask(i) is true then append A(i) and B(i) to C.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! A and B are vectors"
! A = [1, 2]
! B = [3, 4]
! ! Append B(1) and B(2) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .TRUE.])
! CALL OK(ALL(C .EQ. [1, 2, 3, 4]), "Append:")
! ! Append B(1) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append B to A, just assign A to C.
! CALL Append(C, A, B, mask=[.FALSE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! ! A is vector B is scalar
! ! Append 3 to A and then assign result to C
! CALL Append(C, A, 3, mask=.TRUE.)
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append 3 to A, assign result to C, that is C =A
! CALL Append(C, A, 3, mask=.FALSE.)
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_4c(C, A, B, mask)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: C(:)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: B(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4c
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-10-16
! summary: Masked append
!
! If mask(i) is true then append A(i) and B(i) to C.
!
! Example
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! ! A and B are vectors"
! A = [1, 2]
! B = [3, 4]
! ! Append B(1) and B(2) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .TRUE.])
! CALL OK(ALL(C .EQ. [1, 2, 3, 4]), "Append:")
! ! Append B(1) to A and assign to C.
! CALL Append(C, A, B, mask=[.TRUE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append B to A, just assign A to C.
! CALL Append(C, A, B, mask=[.FALSE., .FALSE.])
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! ! A is vector B is scalar
! ! Append 3 to A and then assign result to C
! CALL Append(C, A, 3, mask=.TRUE.)
! CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! ! Do not append 3 to A, assign result to C, that is C =A
! CALL Append(C, A, 3, mask=.FALSE.)
! CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! END PROGRAM main
!```

INTERFACE Append
  MODULE PURE SUBROUTINE Append_4d(C, A, B, mask)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: C(:)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: B(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE Append_4d
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar INT  to a vector of INT.
!
! Append a scalar integer to a vector of INT.
!
! Example:
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP
! USE AppendUtility, ONLY: Append
! USE AppendUtility, ONLY: OPERATOR(.APPEND.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: avec(:)
!
! ! lets us append a single ENTRY to a vector of integers.
! avec = [1] .APPEND.2
! CALL OK(ALL(avec .EQ. [1, 2]), "Append single ENTRY")
!
! ! Appending a vector to a vector.
! avec = avec.APPEND. [3]
! CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")
!
! ! Cleanup
! DEALLOCATE (avec)
! END PROGRAM main
!```

INTERFACE OPERATOR(.APPEND.)
  MODULE PURE FUNCTION func_Append_1a(A, ENTRY) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_1a
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append a scalar REAL to a vector of REAL.

INTERFACE OPERATOR(.APPEND.)
  MODULE PURE FUNCTION func_Append_1b(A, ENTRY) RESULT(ans)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-16
! summary: Append two vectors of INTEGER

INTERFACE OPERATOR(.APPEND.)
  MODULE PURE FUNCTION func_Append_2a(A, ENTRY) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: ENTRY(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_2a
END INTERFACE OPERATOR(.APPEND.)

!----------------------------------------------------------------------------
!                                                             Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Append two vectors of REAL

INTERFACE OPERATOR(.APPEND.)
  MODULE PURE FUNCTION func_Append_2b(A, ENTRY) RESULT(ans)
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), INTENT(IN) :: ENTRY(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION func_Append_2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1a(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a(:)
    REAL(REAL32), INTENT(IN) :: b(:)
    REAL(REAL32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1a
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1b(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a(:)
    REAL(REAL64), INTENT(IN) :: b(:)
    REAL(REAL64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1b
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1c(a, b) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: a(:)
    INTEGER(INT64), INTENT(IN) :: b(:)
    INTEGER(INT64), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1c
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1d(a, b) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: a(:)
    INTEGER(INT32), INTENT(IN) :: b(:)
    INTEGER(INT32), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1d
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1e(a, b) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: a(:)
    INTEGER(INT16), INTENT(IN) :: b(:)
    INTEGER(INT16), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1e
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors
!
! Concat columns of two vectors, a and b and return a matrix with two columns.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! ! USE Display_Method, ONLY: Display
! USE String_Class, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! ! Concat two integer vectors
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! ! Concat two integer vectors of different length
! CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! ! Concat two integer vectors of different length.
! CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
!              "colconcat="//CHAR_LF//CHAR_LF)
! END PROGRAM main
!```

INTERFACE ColConcat
  MODULE PURE FUNCTION colConcat_1f(a, b) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: a(:)
    INTEGER(INT8), INTENT(IN) :: b(:)
    INTEGER(INT8), ALLOCATABLE :: ans(:, :)
  END FUNCTION colConcat_1f
END INTERFACE ColConcat

!----------------------------------------------------------------------------
!                                                          ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of two vectors

INTERFACE OPERATOR(.ColConcat.)
  MODULE PROCEDURE colConcat_1a, colConcat_1b, colConcat_1c, &
    colConcat_1d, colConcat_1e, colConcat_1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of a matrix and a vector
!
! Concat columns of a matrix and a vector, a and b, and return a matrix with
! columns of a and vector b.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=[5, 6]), "colconcat=")
!
! ! ColConcat works fine even when number of rows in rank-2
! ! array is not the same as the size of rank-1 array.
!
! CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=[5, 6, 7]), "colconcat=")
!
! ! Results
! !
! ! colconcat=
! ! ----------
! !  1  3  5
! !  2  4  6
! !  0  0  7
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        a=[5, 6]), "colconcat=")
!
! ! colconcat=
! ! ----------
! !  5  1  3
! !  6  2  4
!
! ! ColConcat works fine even when number of rows in rank-2 array is not the
! ! same as the size of rank-1 array.
! CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        a=[5, 6, 7]), "colconcat=")
!
! ! colconcat=
! ! ----------
! !  5  1  3
! !  6  2  4
! !  7  0  0
! END PROGRAM main
!```

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
    colConcat_2d, colConcat_2e, colConcat_2f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat columns of rank1 and rank2 array
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE MdEncode_Method, ONLY: MdEnCode
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=[5, 6]), "colconcat=")
!
! ! ColConcat works fine even when number of rows in rank-2
! ! array is not the same as the size of rank-1 array.
!
! CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=[5, 6, 7]), "colconcat=")
!
! ! Results
! !
! ! colconcat=
! ! ----------
! !  1  3  5
! !  2  4  6
! !  0  0  7
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        a=[5, 6]), "colconcat=")
!
! ! colconcat=
! ! ----------
! !  5  1  3
! !  6  2  4
!
! ! ColConcat works fine even when number of rows in rank-2 array is not the
! ! same as the size of rank-1 array.
! CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        a=[5, 6, 7]), "colconcat=")
!
! ! colconcat=
! ! ----------
! !  5  1  3
! !  6  2  4
! !  7  0  0
! END PROGRAM main
!```

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
! summary: Concat columns of rank2 and rank2 array
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: ColConcat
! USE Display_Method, ONLY: Display
! USE ArangeUtility, ONLY: Arange
! IMPLICIT NONE
!
! CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=RESHAPE([5, 6, 7, 8], [2, 2])), "colconcat=")
!
! ! colconcat=
! ! ----------
! ! 1  3  5  7
! ! 2  4  6  8
!
! CALL Display(ColConcat(a=RESHAPE(arange(1, 9), [3, 3]), &
!                        b=RESHAPE(arange(10, 13), [2, 2])), "colconcat=")
!
! ! ```txt title="results"
! !     colconcat=
! ! ------------------
! !  1   4   7  10  12
! !  2   5   8  11  13
! !  3   6   9   0   0
!
! CALL Display(ColConcat(a=RESHAPE(arange(1, 4), [2, 2]), &
!                        b=RESHAPE(arange(5, 13), [3, 3])), "colconcat=")
!
! ! ```txt title="results"
! !     colconcat=
! ! ------------------
! !  1   3   5   8  11
! !  2   4   6   9  12
! !  0   0   7  10  13
! ! ```
!
! END PROGRAM main
!```

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
    colConcat_4d, colConcat_4e, colConcat_4f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ColConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat rows of two vectors
!
! Concat rows of two vectors, a and b and return a matrix with two rows.
!
! Examples
!
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: RowConcat
! USE Display_Method, ONLY: Display
! USE Test_Method, ONLY: ok
! IMPLICIT NONE
!
! ! Concat two integer vectors
! CALL Display(RowConcat(a=[1, 2], b=[3, 4]), "RowConcat=")
! CALL OK(ALL(RowConcat(a=[1, 2], b=[3, 4]) &
!             .EQ. RESHAPE([1, 3, 2, 4], [2, 2])), "RowConcat")
!
! ! RowConcat=
! ! ----------
! !    1  2
! !    3  4
!
! ! Concat two integer vectors of different length
! CALL Display(RowConcat(a=[1, 2], b=[3, 4, 5]), "RowConcat=")
! CALL OK(ALL(RowConcat(a=[1, 2], b=[3, 4, 5]) &
!             .EQ. RESHAPE([1, 3, 2, 4, 0, 5], [2, 3])), "RowConcat")
!
! ! RowConcat=
! ! ----------
! !  1  2  0
! !  3  4  5
! END PROGRAM main
!```

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
    rowConcat_1d, rowConcat_1e, rowConcat_1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat rows of a matrix and a vector
!
! Cocat rows of a matrix and a vector, a and b, and return a matrix with rows
! of a and vector b.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: RowConcat
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        b=[3, 6]), "RowConcat=")
!
! ! RowConcat=
! ! ----------
! !    1  4
! !    2  5
! !    3  6
!
! ! RowConcat works fine even when number of columns in rank-2 array
! ! is not the same as the size of rank-1 array.
!
! CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        b=[3, 6, 7]), "RowConcat=")
!
! ! RowConcat=
! ! ----------
! !  1  4  0
! !  2  5  0
! !  3  6  7
!
! ! We can also concat the columns of a vector and a matrix.
! CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        a=[3, 6]), "RowConcat=")
! ! RowConcat=
! ! ----------
! !    3  6
! !    1  4
! !    2  5
!
! CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        a=[3, 6, 7]), "RowConcat=")
! ! RowConcat=
! ! ----------
! !  3  6  7
! !  1  4  0
! !  2  5  0
! END PROGRAM main
!```

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
    rowConcat_2d, rowConcat_2e, rowConcat_2f
END INTERFACE

!----------------------------------------------------------------------------
!                                                    rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat rows of rank1 and rank2 array
!
! Concat rows of a vector and a matrix, a and b, and return
! a matrix with rows of a and matrix b.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: RowConcat
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
!
! ! Concat columns of a rank2 array and rank 1 array.
! CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        b=[3, 6]), "RowConcat=")
!
! ! RowConcat=
! ! ----------
! !    1  4
! !    2  5
! !    3  6
!
! ! RowConcat works fine even when number of columns in rank-2 array
! ! is not the same as the size of rank-1 array.
!
! CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        b=[3, 6, 7]), "RowConcat=")
!
! ! RowConcat=
! ! ----------
! !  1  4  0
! !  2  5  0
! !  3  6  7
!
! ! We can also concat the columns of a vector and a matrix.
! CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        a=[3, 6]), "RowConcat=")
! ! RowConcat=
! ! ----------
! !    3  6
! !    1  4
! !    2  5
!
! CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
!                        a=[3, 6, 7]), "RowConcat=")
! ! RowConcat=
! ! ----------
! !  3  6  7
! !  1  4  0
! !  2  5  0
! END PROGRAM main
!```

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
!                                                          rowConcat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Concat rows of rank2 and rank2 array
!
! Concat rows of rank2 and rank2 array.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
! USE AppendUtility, ONLY: RowConcat
! USE Display_Method, ONLY: Display
! USE ArangeUtility, ONLY: Arange
! IMPLICIT NONE
! !
! ! We can also concat the columns of two matrices.
! CALL Display(RowConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
!                        b=RESHAPE([5, 6, 7, 8], [2, 2])), "RowConcat=")
! !
! ! RowConcat=
! ! ----------
! !    1  3
! !    2  4
! !    5  7
! !    6  8
! !
! CALL Display(RowConcat(a=RESHAPE(Arange(1, 9), [3, 3]), &
!                        b=RESHAPE(Arange(10, 13), [2, 2])), "RowConcat=")
! !
! ! RowConcat=
! ! ----------
! !  1   4   7
! !  2   5   8
! !  3   6   9
! ! 10  12   0
! ! 11  13   0
! !
! CALL Display(RowConcat(a=RESHAPE(Arange(1, 4), [2, 2]), &
!                        b=RESHAPE(Arange(5, 13), [3, 3])), "RowConcat=")
! !
! ! RowConcat=
! ! ----------
! !  1   3   0
! !  2   4   0
! !  5   8  11
! !  6   9  12
! !  7  10  13
! END PROGRAM main
!```

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE OPERATOR(.rowConcat.)
  MODULE PROCEDURE rowConcat_4a, rowConcat_4b, rowConcat_4c, &
    rowConcat_4d, rowConcat_4e, rowConcat_4f
END INTERFACE

END MODULE AppendUtility
