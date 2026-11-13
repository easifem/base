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
! summary: Returns the first element of an array or a string.
!
! Description:
!   This module contains the generic function `HEAD`, which returns the
!   first element of a vector, or the first character of a string.
!
!   `HEAD` is available for
!
!   - integer vectors (`INT8`, `INT16`, `INT32`, `INT64`),
!   - real vectors (`REAL32`, `REAL64`),
!   - strings (`CHARACTER(*)`).

MODULE HeadUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: HEAD

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first element of an integer vector.
!
! Description:
!   Returns `x(1)`, where `x` is a vector of `INT8`, `INT16`, `INT32`
!   or `INT64`. The result has the same kind as `x`.
!
!   The size of `x` should be at least one. This is not checked, so
!   an empty vector gives an undefined result.
!
! Arguments:
!   x : INTEGER, rank(1) - input vector, `SIZE(x) >= 1`
!
! Returns:
!   ans : INTEGER - the first element of `x`, same kind as `x`
!
! Formula:
!   $$\mathrm{ans} = x_1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE HeadUtility, ONLY: HEAD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(4), y(1), z(3)
!
! x = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
! CALL OK(HEAD(x) .EQ. 10_I4B, "head of a vector")
! CALL OK(HEAD(x) .EQ. x(1), "head is x(1)")
! CALL OK(HEAD(x) .NE. x(SIZE(x)), "head is not the last element")
!
! ! array section
! CALL OK(HEAD(x(2:3)) .EQ. 20_I4B, "head of x(2:3)")
!
! ! vector with a single element
! y = [7_I4B]
! CALL OK(HEAD(y) .EQ. 7_I4B, "head of a vector of size 1")
!
! ! negative values
! z = [-5_I4B, 0_I4B, 5_I4B]
! CALL OK(HEAD(z) .EQ. -5_I4B, "head of a vector with negative entry")
!
! END PROGRAM main
!```

INTERFACE HEAD
  MODULE PURE FUNCTION head_Int8(x) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: ans
  END FUNCTION head_Int8

  MODULE PURE FUNCTION head_Int16(x) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: ans
  END FUNCTION head_Int16

  MODULE PURE FUNCTION head_Int32(x) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: ans
  END FUNCTION head_Int32

  MODULE PURE FUNCTION head_Int64(x) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: ans
  END FUNCTION head_Int64
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first element of a real vector.
!
! Description:
!   Returns `x(1)`, where `x` is a vector of `REAL32` or `REAL64`. The
!   result has the same kind as `x`.
!
!   The size of `x` should be at least one. This is not checked, so
!   an empty vector gives an undefined result.
!
! Arguments:
!   x : REAL, rank(1) - input vector, `SIZE(x) >= 1`
!
! Returns:
!   ans : REAL - the first element of `x`, same kind as `x`
!
! Formula:
!   $$\mathrm{ans} = x_1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE HeadUtility, ONLY: HEAD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: x(4), y(1), z(3)
!
! x = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP]
! CALL OK(ABS(HEAD(x) - 1.5_DFP) .LE. tol, "head of a vector")
! CALL OK(HEAD(x) .EQ. x(1), "head is x(1)")
! CALL OK(HEAD(x) .NE. x(SIZE(x)), "head is not the last element")
!
! ! array section
! CALL OK(ABS(HEAD(x(2:3)) - 2.5_DFP) .LE. tol, "head of x(2:3)")
!
! ! vector with a single element
! y = [7.25_DFP]
! CALL OK(ABS(HEAD(y) - 7.25_DFP) .LE. tol, "head of a vector of size 1")
!
! ! negative values
! z = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL OK(ABS(HEAD(z) + 0.5_DFP) .LE. tol, "head with negative entry")
!
! END PROGRAM main
!```

INTERFACE HEAD
  MODULE PURE FUNCTION head_Real32(x) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans
  END FUNCTION head_Real32

  MODULE PURE FUNCTION head_Real64(x) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans
  END FUNCTION head_Real64
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                                      Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns the first character of a string.
!
! Description:
!   Returns `x(1:1)`, the first character of the string `x`. The result
!   always has length one.
!
!   - The case of the character is preserved.
!   - A leading blank is returned as a blank.
!   - The length of `x` should be at least one. This is not checked, so
!     an empty string gives an undefined result.
!
! Arguments:
!   x : CHARACTER(*) - input string, `LEN(x) >= 1`
!
! Returns:
!   ans : CHARACTER(1) - the first character of `x`
!
! Formula:
!   $$\mathrm{ans} = x_{1:1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE HeadUtility, ONLY: HEAD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=16) :: aline
!
! CALL OK(HEAD("Hello") .EQ. "H", "head of Hello")
! CALL OK(HEAD("a") .EQ. "a", "head of a string of length 1")
! CALL OK(LEN(HEAD("abc")) .EQ. 1_I4B, "result has length 1")
!
! ! a leading blank is the head
! CALL OK(HEAD(" x") .EQ. " ", "head is a leading blank")
!
! ! a variable with a trailing blank padding
! aline = "World"
! CALL OK(HEAD(aline) .EQ. "W", "head of a padded variable")
! CALL OK(HEAD(TRIM(aline)) .EQ. "W", "head of TRIM(aline)")
!
! ! case is preserved
! CALL OK(HEAD("hello") .NE. HEAD("Hello"), "case matters")
!
! END PROGRAM main
!```

INTERFACE HEAD
  MODULE PURE FUNCTION head_char(x) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(1) :: ans
  END FUNCTION head_char
END INTERFACE HEAD

END MODULE HeadUtility
