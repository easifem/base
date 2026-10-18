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
! date: 2021-04-03
! summary: Returns an optional argument or its default value.
!
! Description:
!   This module contains the generic function `Input`, which handles
!   optional arguments. It returns `option` when it is present, and
!   `default` otherwise.
!
!```fortran
! a = Input(default=default, option=option)
!```
!
!   `a`, `default` and `option` should have the same type, kind and
!   shape. `Input` is available for
!
!   - integer scalars, vectors and matrices
!     (`INT8`, `INT16`, `INT32`, `INT64`),
!   - real scalars, vectors and matrices (`REAL32`, `REAL64`),
!   - logical scalars, vectors and matrices (`LGT`),
!   - strings (`CHARACTER(*)`).

MODULE InputUtility
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional integer scalar or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`. The result
!   has the same kind as `default`.
!
! Arguments:
!   default : INTEGER - value returned when `option` is absent
!   option : INTEGER, OPTIONAL - value returned when it is present,
!     same kind as `default`
!
! Returns:
!   ans : INTEGER - `option` or `default`, same kind as `default`
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{option}, & \text{if option is present} \\
!     \mathrm{default}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a, b
!
! a = 10_I4B
! b = 20_I4B
!
! ! option is present
! CALL OK(Input(a, b) .EQ. 20_I4B, "option is present")
! CALL OK(Input(default=a, option=b) .EQ. b, "keyword arguments")
!
! ! option is absent
! CALL OK(Input(a) .EQ. 10_I4B, "option is absent")
! CALL OK(Input(default=5_I4B) .EQ. 5_I4B, "literal default")
!
! ! option is present and equal to zero
! CALL OK(Input(a, 0_I4B) .EQ. 0_I4B, "option is zero")
!
! ! negative values
! CALL OK(Input(-3_I4B, -7_I4B) .EQ. -7_I4B, "negative option")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Int8(default, option) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: default
    INTEGER(INT8), OPTIONAL, INTENT(IN) :: option
    INTEGER(INT8) :: Ans
  END FUNCTION input_Int8

  MODULE PURE FUNCTION input_Int16(default, option) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: default
    INTEGER(INT16), OPTIONAL, INTENT(IN) :: option
    INTEGER(INT16) :: Ans
  END FUNCTION input_Int16

  MODULE PURE FUNCTION input_Int32(default, option) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: default
    INTEGER(INT32), OPTIONAL, INTENT(IN) :: option
    INTEGER(INT32) :: Ans
  END FUNCTION input_Int32

  MODULE PURE FUNCTION input_Int64(default, option) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: default
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: option
    INTEGER(INT64) :: Ans
  END FUNCTION input_Int64
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional real scalar or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `REAL32` and `REAL64`. The result has the same
!   kind as `default`.
!
! Arguments:
!   default : REAL - value returned when `option` is absent
!   option : REAL, OPTIONAL - value returned when it is present,
!     same kind as `default`
!
! Returns:
!   ans : REAL - `option` or `default`, same kind as `default`
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{option}, & \text{if option is present} \\
!     \mathrm{default}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a, b
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = 1.5_DFP
! b = 2.5_DFP
!
! ! option is present
! CALL OK(ABS(Input(a, b) - 2.5_DFP) .LT. tol, "option is present")
! CALL OK(ABS(Input(default=a, option=b) - b) .LT. tol, &
!         "keyword arguments")
!
! ! option is absent
! CALL OK(ABS(Input(a) - 1.5_DFP) .LT. tol, "option is absent")
! CALL OK(ABS(Input(default=0.25_DFP) - 0.25_DFP) .LT. tol, &
!         "literal default")
!
! ! option is present and equal to zero
! CALL OK(ABS(Input(a, 0.0_DFP)) .LT. tol, "option is zero")
!
! ! negative values
! CALL OK(ABS(Input(-3.0_DFP, -7.5_DFP) + 7.5_DFP) .LT. tol, &
!         "negative option")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Real32(default, option) RESULT(ans)
    REAL(REAL32), INTENT(in) :: default
    REAL(REAL32), OPTIONAL, INTENT(in) :: option
    REAL(REAL32) :: ans
  END FUNCTION input_Real32

  MODULE PURE FUNCTION input_Real64(default, option) RESULT(ans)
    REAL(REAL64), INTENT(in) :: default
    REAL(REAL64), OPTIONAL, INTENT(in) :: option
    REAL(REAL64) :: ans
  END FUNCTION input_Real64
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional integer vector or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - The result has the same kind and size as `default`.
!   - `option` should have the same size as `default`. This is not
!     checked, so a different size gives an undefined result.
!
! Arguments:
!   default : INTEGER, rank(1) - value returned when `option` is absent
!   option : INTEGER, rank(1), OPTIONAL - value returned when it is
!     present, `SIZE(option) = SIZE(default)`
!
! Returns:
!   ans : INTEGER, rank(1) - `option` or `default`,
!     `SIZE(ans) = SIZE(default)`
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!     \mathrm{option}_i, & \text{if option is present} \\
!     \mathrm{default}_i, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(3), b(3), c(3)
!
! a = [1_I4B, 2_I4B, 3_I4B]
! b = [4_I4B, 5_I4B, 6_I4B]
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(c .EQ. b), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(c .EQ. a), "option is absent")
!
! ! the size of the result is the size of default
! CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
! CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")
!
! ! array sections
! CALL OK(ALL(Input(a(1:2), b(2:3)) .EQ. [5_I4B, 6_I4B]), &
!         "option is an array section")
! CALL OK(ALL(Input(a(2:3)) .EQ. [2_I4B, 3_I4B]), &
!         "default is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Int8Vec(default, option) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: default(:)
    INTEGER(INT8), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(INT8) :: ans(SIZE(default))
  END FUNCTION input_Int8Vec

  MODULE PURE FUNCTION input_Int16Vec(default, option) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: default(:)
    INTEGER(INT16), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(INT16) :: ans(SIZE(default))
  END FUNCTION input_Int16Vec

  MODULE PURE FUNCTION input_Int32Vec(default, option) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: default(:)
    INTEGER(INT32), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(INT32) :: ans(SIZE(default))
  END FUNCTION input_Int32Vec

  MODULE PURE FUNCTION input_Int64Vec(default, option) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: default(:)
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(INT64) :: ans(SIZE(default))
  END FUNCTION input_Int64Vec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional real vector or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `REAL32` and `REAL64`.
!
!   - The result has the same kind and size as `default`.
!   - `option` should have the same size as `default`. This is not
!     checked, so a different size gives an undefined result.
!
! Arguments:
!   default : REAL, rank(1) - value returned when `option` is absent
!   option : REAL, rank(1), OPTIONAL - value returned when it is
!     present, `SIZE(option) = SIZE(default)`
!
! Returns:
!   ans : REAL, rank(1) - `option` or `default`,
!     `SIZE(ans) = SIZE(default)`
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!     \mathrm{option}_i, & \text{if option is present} \\
!     \mathrm{default}_i, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3), b(3), c(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! b = [4.5_DFP, 5.5_DFP, 6.5_DFP]
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(ABS(c - b) .LT. tol), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(ABS(c - a) .LT. tol), "option is absent")
!
! ! the size of the result is the size of default
! CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
! CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")
!
! ! array sections
! CALL OK(ALL(ABS(Input(a(1:2), b(2:3)) - [5.5_DFP, 6.5_DFP]) &
!         .LT. tol), "option is an array section")
! CALL OK(ALL(ABS(Input(a(2:3)) - [2.0_DFP, 3.0_DFP]) .LT. tol), &
!         "default is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Real32vec(default, option) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: default(:)
    REAL(REAL32), OPTIONAL, INTENT(IN) :: option(:)
    REAL(REAL32) :: ans(SIZE(default))
  END FUNCTION input_Real32vec

  MODULE PURE FUNCTION input_Real64vec(default, option) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: default(:)
    REAL(REAL64), OPTIONAL, INTENT(IN) :: option(:)
    REAL(REAL64) :: ans(SIZE(default))
  END FUNCTION input_Real64vec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional integer matrix or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - The result has the same kind and shape as `default`.
!   - `option` should have the same shape as `default`. This is not
!     checked, so a different shape gives an undefined result.
!
! Arguments:
!   default : INTEGER, rank(2) - value returned when `option` is absent
!   option : INTEGER, rank(2), OPTIONAL - value returned when it is
!     present, `SHAPE(option) = SHAPE(default)`
!
! Returns:
!   ans : INTEGER, rank(2) - `option` or `default`,
!     `SHAPE(ans) = SHAPE(default)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \begin{cases}
!     \mathrm{option}_{ij}, & \text{if option is present} \\
!     \mathrm{default}_{ij}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: a(2, 3), b(2, 3), c(2, 3)
!
! a = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], &
!             [2_I4B, 3_I4B])
! b = 10_I4B * a
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(c .EQ. b), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(c .EQ. a), "option is absent")
!
! ! the shape of the result is the shape of default
! CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result")
! CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result with option")
!
! ! array sections
! CALL OK(ALL(Input(a(:, 1:2), b(:, 2:3)) .EQ. b(:, 2:3)), &
!         "option is an array section")
! CALL OK(ALL(Input(a(:, 2:3)) .EQ. a(:, 2:3)), &
!         "default is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Int8Array(default, option) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: default(:, :)
    INTEGER(INT8), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(INT8) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int8Array

  MODULE PURE FUNCTION input_Int16Array(default, option) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: default(:, :)
    INTEGER(INT16), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(INT16) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int16Array

  MODULE PURE FUNCTION input_Int32Array(default, option) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: default(:, :)
    INTEGER(INT32), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(INT32) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int32Array

  MODULE PURE FUNCTION input_Int64Array(default, option) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: default(:, :)
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(INT64) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int64Array
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional real matrix or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   It is available for `REAL32` and `REAL64`.
!
!   - The result has the same kind and shape as `default`.
!   - `option` should have the same shape as `default`. This is not
!     checked, so a different shape gives an undefined result.
!
! Arguments:
!   default : REAL, rank(2) - value returned when `option` is absent
!   option : REAL, rank(2), OPTIONAL - value returned when it is
!     present, `SHAPE(option) = SHAPE(default)`
!
! Returns:
!   ans : REAL, rank(2) - `option` or `default`,
!     `SHAPE(ans) = SHAPE(default)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \begin{cases}
!     \mathrm{option}_{ij}, & \text{if option is present} \\
!     \mathrm{default}_{ij}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(2, 3), b(2, 3), c(2, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
!             [2_I4B, 3_I4B])
! b = 0.5_DFP * a
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(ABS(c - b) .LT. tol), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(ABS(c - a) .LT. tol), "option is absent")
!
! ! the shape of the result is the shape of default
! CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result")
! CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result with option")
!
! ! array sections
! CALL OK(ALL(ABS(Input(a(:, 1:2), b(:, 2:3)) - b(:, 2:3)) .LT. tol), &
!         "option is an array section")
! CALL OK(ALL(ABS(Input(a(:, 2:3)) - a(:, 2:3)) .LT. tol), &
!         "default is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_Real32Array(default, option) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: default(:, :)
    REAL(REAL32), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(REAL32) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Real32Array

  MODULE PURE FUNCTION input_Real64Array(default, option) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: default(:, :)
    REAL(REAL64), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(REAL64) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Real64Array
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional string or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!
!   - The result is allocated to the length of the selected string.
!   - Trailing blanks are kept; the result is not trimmed.
!   - An empty `option` is still present, so it gives an empty result.
!
! Arguments:
!   default : CHARACTER(*) - value returned when `option` is absent
!   option : CHARACTER(*), OPTIONAL - value returned when it is present
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - `option` or `default`,
!     `LEN(ans) = LEN(option)` if `option` is present, otherwise
!     `LEN(ans) = LEN(default)`
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{option}, & \text{if option is present} \\
!     \mathrm{default}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(:), ALLOCATABLE :: s
!
! ! option is present
! s = Input("abc", "xyz")
! CALL OK(s .EQ. "xyz", "option is present")
!
! ! option is absent
! s = Input("abc")
! CALL OK(s .EQ. "abc", "option is absent")
! CALL OK(LEN(s) .EQ. 3_I4B, "length of default")
!
! ! the result has the length of the selected string
! s = Input("abc", "hello")
! CALL OK(s .EQ. "hello", "longer option")
! CALL OK(LEN(s) .EQ. 5_I4B, "length of longer option")
!
! ! trailing blanks of the option are kept
! s = Input("abc", "xy  ")
! CALL OK(LEN(s) .EQ. 4_I4B, "trailing blanks are kept")
!
! ! an empty option is still an option
! s = Input("abc", "")
! CALL OK(LEN(s) .EQ. 0_I4B, "empty option")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_String(default, option) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: default
    CHARACTER(*), OPTIONAL, INTENT(IN) :: option
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION input_String
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

! INTERFACE
!   MODULE PURE FUNCTION input_StringVec(default, option) RESULT(ans)
!     CHARACTER(*), INTENT(IN) :: default(:)
!     CHARACTER(*), OPTIONAL, INTENT(IN) :: option(:)
!     CHARACTER(:), ALLOCATABLE :: ans(:)
!   END FUNCTION input_StringVec
! END INTERFACE

! INTERFACE Input
!   MODULE PROCEDURE input_StringVec
! END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional logical scalar or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!   This is useful for optional flags, for example
!   `isok = Input(default=.FALSE., option=flag)`.
!
! Arguments:
!   default : LOGICAL(LGT) - value returned when `option` is absent
!   option : LOGICAL(LGT), OPTIONAL - value returned when it is present
!
! Returns:
!   ans : LOGICAL(LGT) - `option` or `default`
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{option}, & \text{if option is present} \\
!     \mathrm{default}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: LGT
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT) :: t, f
!
! t = .TRUE.
! f = .FALSE.
!
! ! option is present
! CALL OK(.NOT. Input(t, f), "option is present (false)")
! CALL OK(Input(f, t), "option is present (true)")
! CALL OK(Input(default=f, option=t), "keyword arguments")
!
! ! option is absent
! CALL OK(Input(t), "option is absent (true)")
! CALL OK(.NOT. Input(f), "option is absent (false)")
! CALL OK(Input(default=t), "keyword default")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_logical(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option
    LOGICAL(LGT) :: ans
  END FUNCTION input_logical
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional logical vector or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!
!   - The result has the same size as `default`.
!   - `option` should have the same size as `default`. This is not
!     checked, so a different size gives an undefined result.
!
! Arguments:
!   default : LOGICAL(LGT), rank(1) - value returned when `option` is
!     absent
!   option : LOGICAL(LGT), rank(1), OPTIONAL - value returned when it
!     is present, `SIZE(option) = SIZE(default)`
!
! Returns:
!   ans : LOGICAL(LGT), rank(1) - `option` or `default`,
!     `SIZE(ans) = SIZE(default)`
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!     \mathrm{option}_i, & \text{if option is present} \\
!     \mathrm{default}_i, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT) :: a(3), b(3), c(3)
!
! a = [.TRUE., .FALSE., .TRUE.]
! b = .NOT. a
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(c .EQV. b), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(c .EQV. a), "option is absent")
!
! ! the size of the result is the size of default
! CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
! CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")
!
! ! array sections
! CALL OK(ALL(Input(a(1:2), b(2:3)) .EQV. b(2:3)), &
!         "option is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_logicalvec(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option(:)
    LOGICAL(LGT) :: ans(SIZE(default))
  END FUNCTION input_logicalvec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns an optional logical matrix or its default value.
!
! Description:
!   Returns `option` if it is present, otherwise returns `default`.
!
!   - The result has the same shape as `default`.
!   - `option` should have the same shape as `default`. This is not
!     checked, so a different shape gives an undefined result.
!
! Arguments:
!   default : LOGICAL(LGT), rank(2) - value returned when `option` is
!     absent
!   option : LOGICAL(LGT), rank(2), OPTIONAL - value returned when it
!     is present, `SHAPE(option) = SHAPE(default)`
!
! Returns:
!   ans : LOGICAL(LGT), rank(2) - `option` or `default`,
!     `SHAPE(ans) = SHAPE(default)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \begin{cases}
!     \mathrm{option}_{ij}, & \text{if option is present} \\
!     \mathrm{default}_{ij}, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: I4B
! USE InputUtility, ONLY: Input
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT) :: a(2, 3), b(2, 3), c(2, 3)
!
! a = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE., .TRUE., .FALSE.], &
!             [2_I4B, 3_I4B])
! b = .NOT. a
!
! ! option is present
! c = Input(a, b)
! CALL OK(ALL(c .EQV. b), "option is present")
!
! ! option is absent
! c = Input(a)
! CALL OK(ALL(c .EQV. a), "option is absent")
!
! ! the shape of the result is the shape of default
! CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result")
! CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
!         "shape of the result with option")
!
! ! array sections
! CALL OK(ALL(Input(a(:, 1:2), b(:, 2:3)) .EQV. b(:, 2:3)), &
!         "option is an array section")
!
! END PROGRAM main
!```

INTERFACE Input
  MODULE PURE FUNCTION input_logicalArray(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default(:, :)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option(:, :)
    LOGICAL(LGT) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_logicalArray
END INTERFACE Input

END MODULE InputUtility
