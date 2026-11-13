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
! date: 2023-06-26
! summary: Routines for creating an identity matrix.
!
! Description:
!   This module contains the generic function `Eye`, which returns an
!   identity matrix of integers or reals.
!
!   - `Eye(m, DataType)` returns an `m x m` identity matrix. The kind of
!     the returned matrix is the kind of `DataType`. The value of
!     `DataType` is not used, it only selects the kind. It can be a
!     scalar of kind `Int8`, `Int16`, `Int32`, `Int64`, `Real32` or
!     `Real64` (and `Int128` when `USE_Int128` is defined).
!   - `Eye(m)` returns an `m x m` identity matrix of `REAL(DFP)`.
!
! Formula:
!   $$I_{ij} = \delta_{ij} = \begin{cases} 1 & i = j \\
!   0 & i \ne j \end{cases}, \qquad 1 \le i, j \le m$$

MODULE EyeUtility
USE GlobalData, ONLY: I4B, LGT, DFP
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
IMPLICIT NONE

PRIVATE
PUBLIC :: Eye

!----------------------------------------------------------------------------
!                                                                Eye@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of integers.
!
! Description:
!   Returns an `m x m` identity matrix of integers.
!
!   - `int_eye_1` returns `INTEGER(INT8)`.
!   - `int_eye_2` returns `INTEGER(INT16)`.
!   - `int_eye_3` returns `INTEGER(INT32)`.
!   - `int_eye_4` returns `INTEGER(INT64)`.
!
!   The kind of the result is the kind of `DataType`. The value of
!   `DataType` is ignored, so it is common to pass `1_Int32`.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows and columns
!   DataType : INTEGER - a scalar of the required kind
!
! Returns:
!   ans : INTEGER, shape (m, m) - identity matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = \delta_{ij}, \qquad 1 \le i, j \le m$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE EyeUtility, ONLY: Eye
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! INTEGER(I4B) :: a(n, n), expected(n, n)
! INTEGER(I4B) :: ii
!
! expected = 0_I4B
! DO ii = 1, n
!   expected(ii, ii) = 1_I4B
! END DO
!
! a = Eye(n, 1_I4B)
!
! CALL OK(ALL(SHAPE(a) .EQ. [n, n]), "Eye(I4B) shape")
! CALL OK(ALL(a .EQ. expected), "Eye(I4B) values")
! CALL OK(SUM(a) .EQ. n, "Eye(I4B) sum is n")
! CALL OK(KIND(a) .EQ. I4B, "Eye(I4B) kind")
!
! END PROGRAM main
!```

INTERFACE Eye
  MODULE PURE FUNCTION int_eye_1(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT8), INTENT(IN) :: DataType
    INTEGER(INT8) :: ans(m, m)
  END FUNCTION int_eye_1

  MODULE PURE FUNCTION int_eye_2(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT16), INTENT(IN) :: DataType
    INTEGER(INT16) :: ans(m, m)
  END FUNCTION int_eye_2

  MODULE PURE FUNCTION int_eye_3(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT32), INTENT(IN) :: DataType
    INTEGER(INT32) :: ans(m, m)
  END FUNCTION int_eye_3

  MODULE PURE FUNCTION int_eye_4(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT64), INTENT(IN) :: DataType
    INTEGER(INT64) :: ans(m, m)
  END FUNCTION int_eye_4
END INTERFACE Eye

#ifdef USE_Int128

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of Int128 integers.
!
! Description:
!   Same as `int_eye_1` ... `int_eye_4`, but the result is of kind
!   `Int128`. This routine is available only when `USE_Int128` is
!   defined.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows and columns
!   DataType : INTEGER(Int128) - a scalar, only its kind is used
!
! Returns:
!   ans : INTEGER(Int128), shape (m, m) - identity matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = \delta_{ij}, \qquad 1 \le i, j \le m$$

INTERFACE Eye
  MODULE PURE FUNCTION int_eye_5(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(Int128), INTENT(IN) :: DataType
    INTEGER(Int128) :: ans(m, m)
  END FUNCTION int_eye_5
END INTERFACE Eye
#endif

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of Real64 numbers.
!
! Description:
!   Returns an `m x m` identity matrix of `REAL(REAL64)`. The value of
!   `DataType` is ignored, only its kind is used. Usually, `REAL64` is
!   the same as `DFP`.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows and columns
!   DataType : REAL(REAL64) - a scalar, only its kind is used
!
! Returns:
!   ans : REAL(REAL64), shape (m, m) - identity matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = \delta_{ij}, \qquad 1 \le i, j \le m$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE EyeUtility, ONLY: Eye
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 4
! REAL(DFP) :: a(n, n), expected(n, n)
! INTEGER(I4B) :: ii
!
! expected = 0.0_DFP
! DO ii = 1, n
!   expected(ii, ii) = 1.0_DFP
! END DO
!
! a = Eye(n, 1.0_DFP)
!
! CALL OK(ALL(SHAPE(a) .EQ. [n, n]), "Eye(m, DFP) shape")
! CALL OK(ALL(a .EQ. expected), "Eye(m, DFP) values")
! CALL OK(KIND(a) .EQ. DFP, "Eye(m, DFP) kind")
!
! ! the value of DataType is used only to select the kind
! a = Eye(n, 5.0_DFP)
! CALL OK(ALL(a .EQ. expected), "value of DataType is ignored")
!
! ! same result as Eye(m)
! CALL OK(ALL(Eye(n, 1.0_DFP) .EQ. Eye(n)), "Eye(m, DFP) equals Eye(m)")
!
! END PROGRAM main
!```

INTERFACE Eye
  MODULE PURE FUNCTION real_eye_1(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(REAL64) :: ans(m, m)
    REAL(REAL64), INTENT(IN) :: DataType
  END FUNCTION real_eye_1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of default real numbers.
!
! Description:
!   Returns an `m x m` identity matrix of `REAL(DFP)`. There is no
!   `DataType` argument, so this is the shortest way to get a real
!   identity matrix.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows and columns
!
! Returns:
!   ans : REAL(DFP), shape (m, m) - identity matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = \delta_{ij}, \qquad 1 \le i, j \le m$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE EyeUtility, ONLY: Eye
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP) :: a(n, n), expected(n, n)
! INTEGER(I4B) :: ii
!
! expected = 0.0_DFP
! DO ii = 1, n
!   expected(ii, ii) = 1.0_DFP
! END DO
!
! a = Eye(n)
!
! CALL OK(ALL(SHAPE(a) .EQ. [n, n]), "Eye(m) shape")
! CALL OK(ALL(a .EQ. expected), "Eye(m) values")
! CALL OK(SUM(a) .EQ. REAL(n, DFP), "Eye(m) sum is n")
! CALL OK(KIND(a) .EQ. DFP, "Eye(m) kind")
!
! END PROGRAM main
!```

INTERFACE Eye
  MODULE PURE FUNCTION real_eye_2(m) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(DFP) :: ans(m, m)
  END FUNCTION real_eye_2
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of Real32 numbers.
!
! Description:
!   Same as `real_eye_1`, but the result is of kind `REAL32`. The value
!   of `DataType` is ignored, only its kind is used.
!
! Arguments:
!   m : INTEGER(I4B) - number of rows and columns
!   DataType : REAL(REAL32) - a scalar, only its kind is used
!
! Returns:
!   ans : REAL(REAL32), shape (m, m) - identity matrix
!
! Formula:
!   $$\mathrm{ans}_{ij} = \delta_{ij}, \qquad 1 \le i, j \le m$$

INTERFACE Eye
  MODULE PURE FUNCTION real_eye_3(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(REAL32) :: ans(m, m)
    REAL(REAL32), INTENT(IN) :: DataType
  END FUNCTION real_eye_3
END INTERFACE Eye

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EyeUtility
