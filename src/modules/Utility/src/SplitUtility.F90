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
! summary: Returns the first or the second half of an array or a string.
!
! Description:
!   This module contains the generic function `SPLIT`.
!
!```fortran
! a = SPLIT(x, 1) ! first half of x
! a = SPLIT(x, 2) ! second half of x
!```
!
!   For an odd size the middle entry goes to the second half, so
!   `SPLIT(x, 1)` has `SIZE(x)/2` entries and `SPLIT(x, 2)` has the rest.
!
!   `SPLIT` is available for
!
!   - integer vectors (`INT8`, `INT16`, `INT32`, `INT64`),
!   - real vectors (`REAL32`, `REAL64`),
!   - strings (`CHARACTER(*)`).

MODULE SplitUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE
PUBLIC :: SPLIT

!----------------------------------------------------------------------------
!                                                                     SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first or the second half of an integer vector.
!
! Description:
!   Returns `x(1:n/2)` if `section == 1` and `x(n/2+1:n)` if
!   `section == 2`, where `n = SIZE(x)` and `/` is integer division.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - For an odd `n` the middle entry goes to the second half.
!   - If `n == 1`, `SPLIT(x, 1)` is empty and `SPLIT(x, 2)` is `[x(1)]`.
!   - For any other `section` the result is an empty vector.
!
! Arguments:
!   x : INTEGER, rank(1) - input vector, `n = SIZE(x)`
!   section : INTEGER(I4B) - 1 for the first half, 2 for the second
!
! Returns:
!   ans : INTEGER, ALLOCATABLE, rank(1) - same kind as `x`,
!     `SIZE(ans) = n/2` for section 1 and `n - n/2` for section 2
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     x_{1:\lfloor n/2 \rfloor}, & \text{section} = 1 \\
!     x_{\lfloor n/2 \rfloor + 1:n}, & \text{section} = 2
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SplitUtility, ONLY: Split
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: even(6), odd(5), one(1)
! INTEGER(I4B), ALLOCATABLE :: b(:)
!
! even = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
! odd = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]
! one = [7_I4B]
!
! ! even size: two halves of equal size
! b = Split(even, 1)
! CALL OK(ALL(b .EQ. [1_I4B, 2_I4B, 3_I4B]), "even size, section 1")
! b = Split(even, 2)
! CALL OK(ALL(b .EQ. [4_I4B, 5_I4B, 6_I4B]), "even size, section 2")
!
! ! odd size: the middle entry goes to the second half
! b = Split(odd, 1)
! CALL OK(SIZE(b) .EQ. 2_I4B, "odd size, size of section 1")
! CALL OK(ALL(b .EQ. [1_I4B, 2_I4B]), "odd size, section 1")
! b = Split(odd, 2)
! CALL OK(SIZE(b) .EQ. 3_I4B, "odd size, size of section 2")
! CALL OK(ALL(b .EQ. [3_I4B, 4_I4B, 5_I4B]), "odd size, section 2")
!
! ! size 1: section 1 is empty, section 2 is x(1)
! b = Split(one, 1)
! CALL OK(SIZE(b) .EQ. 0_I4B, "size 1, section 1 is empty")
! b = Split(one, 2)
! CALL OK(SIZE(b) .EQ. 1_I4B, "size 1, size of section 2")
! CALL OK(b(1) .EQ. 7_I4B, "size 1, section 2 is x(1)")
!
! ! the two halves give back the input
! CALL OK(ALL([Split(odd, 1), Split(odd, 2)] .EQ. odd), &
!         "halves joined give the input")
!
! END PROGRAM main
!```

INTERFACE SPLIT
  MODULE PURE FUNCTION split_Int8(x, section) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT8), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int8

  MODULE PURE FUNCTION split_Int16(x, section) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT16), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int16

  MODULE PURE FUNCTION split_Int32(x, section) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT32), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int32

  MODULE PURE FUNCTION split_Int64(x, section) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT64), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int64
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first or the second half of a real vector.
!
! Description:
!   Returns `x(1:n/2)` if `section == 1` and `x(n/2+1:n)` if
!   `section == 2`, where `n = SIZE(x)` and `/` is integer division.
!   It is available for `REAL32` and `REAL64`.
!
!   - For an odd `n` the middle entry goes to the second half.
!   - If `n == 1`, `SPLIT(x, 1)` is empty and `SPLIT(x, 2)` is `[x(1)]`.
!   - For any other `section` the result is an empty vector.
!
! Arguments:
!   x : REAL, rank(1) - input vector, `n = SIZE(x)`
!   section : INTEGER(I4B) - 1 for the first half, 2 for the second
!
! Returns:
!   ans : REAL, ALLOCATABLE, rank(1) - same kind as `x`,
!     `SIZE(ans) = n/2` for section 1 and `n - n/2` for section 2
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     x_{1:\lfloor n/2 \rfloor}, & \text{section} = 1 \\
!     x_{\lfloor n/2 \rfloor + 1:n}, & \text{section} = 2
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE SplitUtility, ONLY: Split
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: areal(10), breal(9)
! REAL(DFP), ALLOCATABLE :: b(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B) :: ii
!
! DO ii = 1, 10
!   areal(ii) = 0.5_DFP * REAL(ii, DFP)
! END DO
! breal = areal(1:9)
!
! ! even size 10: 5 + 5
! b = Split(areal, 1)
! CALL OK(SIZE(b) .EQ. 5_I4B, "even size, size of section 1")
! CALL OK(ALL(ABS(b - areal(:5)) .LT. tol), "even size, section 1")
! b = Split(areal, 2)
! CALL OK(SIZE(b) .EQ. 5_I4B, "even size, size of section 2")
! CALL OK(ALL(ABS(b - areal(6:)) .LT. tol), "even size, section 2")
!
! ! odd size 9: 4 + 5
! b = Split(breal, 1)
! CALL OK(SIZE(b) .EQ. 4_I4B, "odd size, size of section 1")
! CALL OK(ALL(ABS(b - breal(:4)) .LT. tol), "odd size, section 1")
! b = Split(breal, 2)
! CALL OK(SIZE(b) .EQ. 5_I4B, "odd size, size of section 2")
! CALL OK(ALL(ABS(b - breal(5:)) .LT. tol), "odd size, section 2")
!
! ! size 1
! b = Split(areal(3:3), 1)
! CALL OK(SIZE(b) .EQ. 0_I4B, "size 1, section 1 is empty")
! b = Split(areal(3:3), 2)
! CALL OK(ABS(b(1) - 1.5_DFP) .LT. tol, "size 1, section 2 is x(1)")
!
! END PROGRAM main
!```

INTERFACE SPLIT
  MODULE PURE FUNCTION split_Real32(x, section) RESULT(Ans)
    REAL(REAL32), DIMENSION(:), INTENT(IN) :: x
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    REAL(REAL32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_Real32

  MODULE PURE FUNCTION split_Real64(x, section) RESULT(Ans)
    REAL(REAL64), DIMENSION(:), INTENT(IN) :: x
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_Real64
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                                    SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first or the second half of a string.
!
! Description:
!   Returns `x(1:n/2)` if `section == 1` and `x(n/2+1:n)` if
!   `section == 2`, where `n = LEN(x)` and `/` is integer division.
!
!   - For an odd `n` the middle character goes to the second half.
!   - If `n == 1`, `SPLIT(x, 1)` is empty and `SPLIT(x, 2)` is `x`.
!   - For any other `section` the result is an empty string.
!   - Trailing blanks of `x` are counted in `n` and are kept.
!
! Arguments:
!   x : CHARACTER(*) - input string, `n = LEN(x)`
!   section : INTEGER(I4B) - 1 for the first half, 2 for the second
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - `LEN(ans) = n/2` for section 1,
!     `n - n/2` for section 2, and 0 otherwise
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     x_{1:\lfloor n/2 \rfloor}, & \text{section} = 1 \\
!     x_{\lfloor n/2 \rfloor + 1:n}, & \text{section} = 2 \\
!     \text{""}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SplitUtility, ONLY: Split
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(:), ALLOCATABLE :: s
!
! ! even length
! CALL OK(Split("abcdef", 1) .EQ. "abc", "even length, section 1")
! CALL OK(Split("abcdef", 2) .EQ. "def", "even length, section 2")
!
! ! odd length: the middle character goes to the second half
! s = Split("hello", 1)
! CALL OK(s .EQ. "he", "odd length, section 1")
! CALL OK(LEN(s) .EQ. 2_I4B, "odd length, length of section 1")
! s = Split("hello", 2)
! CALL OK(s .EQ. "llo", "odd length, section 2")
! CALL OK(LEN(s) .EQ. 3_I4B, "odd length, length of section 2")
!
! ! length 1
! s = Split("x", 1)
! CALL OK(LEN(s) .EQ. 0_I4B, "length 1, section 1 is empty")
! s = Split("x", 2)
! CALL OK(s .EQ. "x", "length 1, section 2")
!
! ! any other section gives an empty string
! s = Split("abcdef", 3)
! CALL OK(LEN(s) .EQ. 0_I4B, "section 3 gives an empty string")
! s = Split("abcdef", 0)
! CALL OK(LEN(s) .EQ. 0_I4B, "section 0 gives an empty string")
!
! ! trailing blanks are part of the string
! s = Split("ab  ", 2)
! CALL OK(LEN(s) .EQ. 2_I4B, "trailing blanks are kept")
!
! END PROGRAM main
!```

INTERFACE SPLIT
  MODULE PURE FUNCTION split_char(x, section) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: x
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    CHARACTER(:), ALLOCATABLE :: Ans
  END FUNCTION split_char
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SplitUtility
