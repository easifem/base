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
! date: 2023-06-29
! summary: Returns the tail or the last element of an array or a string.
!
! Description:
!   This module contains two generic functions:
!
!   - `TAIL` returns all elements of a vector except the first one,
!     or all characters of a string except the first one.
!   - `LAST` returns the last element of a vector, or the last
!     character of a string.
!
!   Together with `HEAD` (see `HeadUtility`) they split a vector into
!   its first element and the rest. Both are available for
!
!   - integer vectors (`INT8`, `INT16`, `INT32`, `INT64`),
!   - real vectors (`REAL32`, `REAL64`),
!   - strings (`CHARACTER(*)`).

MODULE TailUtility
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: TAIL
PUBLIC :: LAST

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns all but the first element of an integer vector.
!
! Description:
!   Returns `x(2:)`, where `x` is a vector of `INT8`, `INT16`, `INT32`
!   or `INT64`. The result has the same kind as `x` and one element
!   less than `x`.
!
!   A vector of size one, or an empty vector, gives an empty result.
!
! Arguments:
!   x : INTEGER, rank(1) - input vector
!
! Returns:
!   ans : INTEGER, rank(1) - `x(2:)`, same kind as `x`,
!     `SIZE(ans) = MAX(SIZE(x) - 1, 0)`
!
! Formula:
!   $$\mathrm{ans}_i = x_{i+1}, \quad i = 1, \dots, n - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TailUtility, ONLY: TAIL
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(4), y(1)
! INTEGER(I4B), ALLOCATABLE :: z(:)
!
! x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
! CALL OK(ALL(TAIL(x) .EQ. [20_I4B, 30_I4B, 40_I4B]), "tail of a vector")
! CALL OK(ALL(TAIL(x) .EQ. x(2:)), "tail is x(2:)")
! CALL OK(SIZE(TAIL(x)) .EQ. SIZE(x) - 1, "size is SIZE(x) - 1")
!
! ! array section
! CALL OK(ALL(TAIL(x(2:3)) .EQ. [30_I4B]), "tail of x(2:3)")
!
! ! vector with a single element gives an empty vector
! y = [7_I4B]
! CALL OK(SIZE(TAIL(y)) .EQ. 0_I4B, "tail of a vector of size 1")
!
! ! empty vector gives an empty vector
! ALLOCATE (z(0))
! CALL OK(SIZE(TAIL(z)) .EQ. 0_I4B, "tail of an empty vector")
!
! END PROGRAM main
!```

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Int8(x) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int8

  MODULE PURE FUNCTION tail_Int16(x) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int16

  MODULE PURE FUNCTION tail_Int32(x) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int32

  MODULE PURE FUNCTION tail_Int64(x) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns all but the first element of a real vector.
!
! Description:
!   Returns `x(2:)`, where `x` is a vector of `REAL32` or `REAL64`.
!   The result has the same kind as `x` and one element less than
!   `x`.
!
!   A vector of size one, or an empty vector, gives an empty result.
!
! Arguments:
!   x : REAL, rank(1) - input vector
!
! Returns:
!   ans : REAL, rank(1) - `x(2:)`, same kind as `x`,
!     `SIZE(ans) = MAX(SIZE(x) - 1, 0)`
!
! Formula:
!   $$\mathrm{ans}_i = x_{i+1}, \quad i = 1, \dots, n - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TailUtility, ONLY: TAIL
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: x(5), y(1), t(4)
!
! x = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP, 5.5_DFP]
! CALL OK(ALL(ABS(TAIL(x) - x(2:)) .LE. tol), "tail is x(2:)")
! CALL OK(SIZE(TAIL(x)) .EQ. 4_I4B, "size of the tail")
! t = TAIL(x)
! CALL OK(ABS(t(1) - 2.5_DFP) .LE. tol, "first entry of tail")
!
! ! tail of the tail
! CALL OK(ALL(ABS(TAIL(TAIL(x)) - x(3:)) .LE. tol), "tail of tail")
!
! ! vector with a single element gives an empty vector
! y = [7.25_DFP]
! CALL OK(SIZE(TAIL(y)) .EQ. 0_I4B, "tail of a vector of size 1")
!
! END PROGRAM main
!```

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Real32(x) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Real32

  MODULE PURE FUNCTION tail_Real64(x) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Real64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns all but the first character of a string.
!
! Description:
!   Returns `x(2:)`, the string `x` without its first character. The
!   result has length `LEN(x) - 1`.
!
!   - Trailing blanks of `x` are kept; use `TRIM` to drop them.
!   - A string of length one gives an empty string.
!
! Arguments:
!   x : CHARACTER(*) - input string
!
! Returns:
!   ans : CHARACTER(LEN(x) - 1) - `x(2:)`
!
! Formula:
!   $$\mathrm{ans} = x_{2:n}, \quad n = \mathrm{LEN}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TailUtility, ONLY: TAIL
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=8) :: aline
!
! CALL OK(TAIL("Hello") .EQ. "ello", "tail of Hello")
! CALL OK(LEN(TAIL("Hello")) .EQ. 4_I4B, "length is LEN(x) - 1")
!
! ! a string of length 1 gives an empty string
! CALL OK(LEN(TAIL("a")) .EQ. 0_I4B, "tail of a string of length 1")
!
! ! trailing blanks of a padded variable are kept
! aline = "World"
! CALL OK(LEN(TAIL(aline)) .EQ. 7_I4B, "tail keeps trailing blanks")
! CALL OK(TAIL(TRIM(aline)) .EQ. "orld", "tail of TRIM(aline)")
!
! END PROGRAM main
!```

INTERFACE TAIL
  MODULE PURE FUNCTION tail_char(x) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(LEN(x) - 1) :: ans
  END FUNCTION tail_char
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns the last element of an integer vector.
!
! Description:
!   Returns `x(SIZE(x))`, where `x` is a vector of `INT8`, `INT16`,
!   `INT32` or `INT64`. The result has the same kind as `x`.
!
!   The size of `x` should be at least one. This is not checked, so
!   an empty vector gives an undefined result.
!
! Arguments:
!   x : INTEGER, rank(1) - input vector, `SIZE(x) >= 1`
!
! Returns:
!   ans : INTEGER - the last element of `x`, same kind as `x`
!
! Formula:
!   $$\mathrm{ans} = x_n, \quad n = \mathrm{SIZE}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TailUtility, ONLY: LAST
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(4), y(1)
!
! x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
! CALL OK(LAST(x) .EQ. 40_I4B, "last of a vector")
! CALL OK(LAST(x) .EQ. x(SIZE(x)), "last is x(SIZE(x))")
!
! ! array section
! CALL OK(LAST(x(1:2)) .EQ. 20_I4B, "last of x(1:2)")
!
! ! vector with a single element
! y = [7_I4B]
! CALL OK(LAST(y) .EQ. 7_I4B, "last of a vector of size 1")
!
! ! negative values
! CALL OK(LAST([1_I4B, 0_I4B, -5_I4B]) .EQ. -5_I4B, "negative last entry")
!
! END PROGRAM main
!```

INTERFACE LAST
  MODULE PURE FUNCTION last_Int8(x) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: ans
  END FUNCTION last_Int8

  MODULE PURE FUNCTION last_Int16(x) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: ans
  END FUNCTION last_Int16

  MODULE PURE FUNCTION last_Int32(x) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: ans
  END FUNCTION last_Int32

  MODULE PURE FUNCTION last_Int64(x) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: ans
  END FUNCTION last_Int64
END INTERFACE LAST

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns the last element of a real vector.
!
! Description:
!   Returns `x(SIZE(x))`, where `x` is a vector of `REAL32` or
!   `REAL64`. The result has the same kind as `x`.
!
!   The size of `x` should be at least one. This is not checked, so
!   an empty vector gives an undefined result.
!
! Arguments:
!   x : REAL, rank(1) - input vector, `SIZE(x) >= 1`
!
! Returns:
!   ans : REAL - the last element of `x`, same kind as `x`
!
! Formula:
!   $$\mathrm{ans} = x_n, \quad n = \mathrm{SIZE}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE TailUtility, ONLY: LAST
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: x(3), y(1)
!
! x = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! CALL OK(ABS(LAST(x) - 3.0_DFP) .LE. tol, "last of a vector")
! CALL OK(LAST(x) .EQ. x(SIZE(x)), "last is x(SIZE(x))")
!
! ! array section
! CALL OK(ABS(LAST(x(1:2)) - 2.0_DFP) .LE. tol, "last of x(1:2)")
!
! ! vector with a single element
! y = [7.25_DFP]
! CALL OK(ABS(LAST(y) - 7.25_DFP) .LE. tol, "last of a vector of size 1")
!
! END PROGRAM main
!```

INTERFACE LAST
  MODULE PURE FUNCTION last_Real32(x) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans
  END FUNCTION last_Real32

  MODULE PURE FUNCTION last_Real64(x) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans
  END FUNCTION last_Real64
END INTERFACE LAST

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-29
! summary: Returns the last character of a string.
!
! Description:
!   Returns `x(LEN(x):LEN(x))`, the last character of the string `x`.
!   The result always has length one.
!
!   - Trailing blanks count: for a padded variable the result is a
!     blank. Use `LAST(TRIM(x))` to get the last non-blank character.
!   - The length of `x` should be at least one. This is not checked,
!     so an empty string gives an undefined result.
!
! Arguments:
!   x : CHARACTER(*) - input string, `LEN(x) >= 1`
!
! Returns:
!   ans : CHARACTER(1) - the last character of `x`
!
! Formula:
!   $$\mathrm{ans} = x_{n:n}, \quad n = \mathrm{LEN}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TailUtility, ONLY: LAST
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=8) :: aline
!
! CALL OK(LAST("hello world!") .EQ. "!", "last of hello world!")
! CALL OK(LAST("a") .EQ. "a", "last of a string of length 1")
! CALL OK(LEN(LAST("abc")) .EQ. 1_I4B, "result has length 1")
!
! ! a trailing blank is the last character
! CALL OK(LAST("ab ") .EQ. " ", "last is a trailing blank")
!
! ! a padded variable: use TRIM to get the last non-blank character
! aline = "World"
! CALL OK(LAST(aline) .EQ. " ", "last of a padded variable")
! CALL OK(LAST(TRIM(aline)) .EQ. "d", "last of TRIM(aline)")
!
! END PROGRAM main
!```

INTERFACE LAST
  MODULE PURE FUNCTION last_char(x) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(1) :: ans
  END FUNCTION last_char
END INTERFACE LAST

END MODULE TailUtility
