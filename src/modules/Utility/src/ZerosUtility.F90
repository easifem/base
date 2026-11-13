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
! date: 2022-10-29
! summary: Returns arrays with all entries equal to zero.
!
! Description:
!   This module contains the generic function `Zeros`, which returns an
!   array of rank 1 to 4 with every entry equal to zero, similar to
!   `zeros` in Matlab or NumPy.
!
!```fortran
! a = Zeros(dim1, datatype)
! a = Zeros(dim1, dim2, datatype)
! a = Zeros(dim1, dim2, dim3, datatype)
! a = Zeros(dim1, dim2, dim3, dim4, datatype)
! a = Zeros(s, datatype)
!```
!
!   The last argument `datatype` only selects the type and kind of the
!   result. In the last form `s(1:2)` gives the shape of a matrix, for
!   example `Zeros(SHAPE(b), 0.0_DFP)`. `Zeros` is available for
!
!   - integers (`INT8`, `INT16`, `INT32`, `INT64`, and `INT128` when
!     `USE_Int128` is defined),
!   - reals (`REAL32`, `REAL64`).

MODULE ZerosUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
#ifdef USE_Int128
USE GlobalData, ONLY: Int128
#endif
IMPLICIT NONE
PRIVATE

PUBLIC :: Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns an integer vector with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1)` whose entries are all zero.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   datatype : INTEGER - any value of the wanted kind
!
! Returns:
!   ans : INTEGER, rank(1) - shape `(dim1)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i} = 0, \quad i = 1, \dots, \mathrm{dim1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:)
!
! a = Zeros(4_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
!
! ! only the kind of datatype matters, not its value
! a = Zeros(3_I4B, 7_I4B)
! CALL OK(ALL(a .EQ. 0_I4B), "value of datatype is ignored")
! CALL OK(KIND(Zeros(3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")
!
! ! zero size
! a = Zeros(0_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_1(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1)
  END FUNCTION Zeros_1

  MODULE PURE FUNCTION Zeros_2(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1)
  END FUNCTION Zeros_2

  MODULE PURE FUNCTION Zeros_3(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1)
  END FUNCTION Zeros_3

  MODULE PURE FUNCTION Zeros_4(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1)
  END FUNCTION Zeros_4

#ifdef USE_Int128
  MODULE PURE FUNCTION Zeros_5(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1)
  END FUNCTION Zeros_5
#endif
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns a real vector with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1)` whose entries are all zero.
!   It is available for `REAL32` and `REAL64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   datatype : REAL - any value of the wanted kind
!
! Returns:
!   ans : REAL, rank(1) - shape `(dim1)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i} = 0, \quad i = 1, \dots, \mathrm{dim1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Zeros(4_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
! CALL OK(ALL(ABS(a) .LT. tol), "all entries are zero")
!
! ! only the kind of datatype matters, not its value
! a = Zeros(3_I4B, 5.0_DFP)
! CALL OK(ALL(ABS(a) .LT. tol), "value of datatype is ignored")
! CALL OK(KIND(Zeros(3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")
!
! ! zero size
! a = Zeros(0_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_6(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1)
  END FUNCTION Zeros_6

  MODULE PURE FUNCTION Zeros_7(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1)
  END FUNCTION Zeros_7
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns an integer matrix with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2)` whose entries are all zero.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   datatype : INTEGER - any value of the wanted kind
!
! Returns:
!   ans : INTEGER, rank(2) - shape `(dim1, dim2)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :)
!
! a = Zeros(2_I4B, 3_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
! CALL OK(KIND(Zeros(2_I4B, 3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")
!
! ! zero size in one dimension
! a = Zeros(2_I4B, 0_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_8(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2)
  END FUNCTION Zeros_8

  MODULE PURE FUNCTION Zeros_9(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2)
  END FUNCTION Zeros_9

  MODULE PURE FUNCTION Zeros_10(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2)
  END FUNCTION Zeros_10

  MODULE PURE FUNCTION Zeros_11(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2)
  END FUNCTION Zeros_11

#ifdef USE_Int128
  MODULE PURE FUNCTION Zeros_12(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2)
  END FUNCTION Zeros_12
#endif
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns a real matrix with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2)` whose entries are all zero.
!   It is available for `REAL32` and `REAL64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   datatype : REAL - any value of the wanted kind
!
! Returns:
!   ans : REAL, rank(2) - shape `(dim1, dim2)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Zeros(2_I4B, 3_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
! CALL OK(ALL(ABS(a) .LT. tol), "all entries are zero")
! CALL OK(KIND(Zeros(2_I4B, 3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")
!
! ! zero size in one dimension
! a = Zeros(0_I4B, 3_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_13(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2)
  END FUNCTION Zeros_13

  MODULE PURE FUNCTION Zeros_14(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2)
  END FUNCTION Zeros_14
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns an integer rank-3 array with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3)` whose entries are all zero.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   dim3 : INTEGER(I4B) - extent of dimension 3
!   datatype : INTEGER - any value of the wanted kind
!
! Returns:
!   ans : INTEGER, rank(3) - shape `(dim1, dim2, dim3)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j,k} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :)
!
! a = Zeros(2_I4B, 3_I4B, 4_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape of the result")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
! CALL OK(SUM(a) .EQ. 0_I4B, "sum of entries is zero")
! CALL OK(KIND(Zeros(1_I4B, 1_I4B, 1_I4B, 0_I4B)) .EQ. I4B, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_15(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_15

  MODULE PURE FUNCTION Zeros_16(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_16

  MODULE PURE FUNCTION Zeros_17(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_17

  MODULE PURE FUNCTION Zeros_18(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_18

#ifdef USE_Int128
  MODULE PURE FUNCTION Zeros_19(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_19
#endif
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns a real rank-3 array with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3)` whose entries are all zero.
!   It is available for `REAL32` and `REAL64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   dim3 : INTEGER(I4B) - extent of dimension 3
!   datatype : REAL - any value of the wanted kind
!
! Returns:
!   ans : REAL, rank(3) - shape `(dim1, dim2, dim3)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j,k} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Zeros(2_I4B, 3_I4B, 4_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape of the result")
! CALL OK(ALL(ABS(a) .LT. tol), "all entries are zero")
! CALL OK(ABS(SUM(a)) .LT. tol, "sum of entries is zero")
! CALL OK(KIND(Zeros(1_I4B, 1_I4B, 1_I4B, 0.0_DFP)) .EQ. DFP, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_20(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_20

  MODULE PURE FUNCTION Zeros_21(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_21
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns an integer rank-4 array with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3, dim4)` whose entries
!   are all zero.
!   It is available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   dim3 : INTEGER(I4B) - extent of dimension 3
!   dim4 : INTEGER(I4B) - extent of dimension 4
!   datatype : INTEGER - any value of the wanted kind
!
! Returns:
!   ans : INTEGER, rank(4) - shape `(dim1, dim2, dim3, dim4)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j,k,l} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :)
!
! a = Zeros(2_I4B, 3_I4B, 4_I4B, 5_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "shape of the result")
! CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
! CALL OK(SUM(a) .EQ. 0_I4B, "sum of entries is zero")
! CALL OK(KIND(Zeros(1_I4B, 1_I4B, 1_I4B, 1_I4B, 0_I4B)) .EQ. I4B, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_22(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_22

  MODULE PURE FUNCTION Zeros_23(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_23

  MODULE PURE FUNCTION Zeros_24(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_24

  MODULE PURE FUNCTION Zeros_25(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_25

#ifdef USE_Int128
  MODULE PURE FUNCTION Zeros_26(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_26
#endif
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns a real rank-4 array with all entries equal to zero.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3, dim4)` whose entries
!   are all zero.
!   It is available for `REAL32` and `REAL64`.
!
!   - `datatype` only selects the kind of the result. Its value is
!     not used.
!   - A zero extent gives an empty array.
!
! Arguments:
!   dim1 : INTEGER(I4B) - extent of dimension 1
!   dim2 : INTEGER(I4B) - extent of dimension 2
!   dim3 : INTEGER(I4B) - extent of dimension 3
!   dim4 : INTEGER(I4B) - extent of dimension 4
!   datatype : REAL - any value of the wanted kind
!
! Returns:
!   ans : REAL, rank(4) - shape `(dim1, dim2, dim3, dim4)`, same kind as
!     `datatype`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j,k,l} = 0 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Zeros(2_I4B, 3_I4B, 4_I4B, 5_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "shape of the result")
! CALL OK(ALL(ABS(a) .LT. tol), "all entries are zero")
! CALL OK(ABS(SUM(a)) .LT. tol, "sum of entries is zero")
! CALL OK(KIND(Zeros(1_I4B, 1_I4B, 1_I4B, 1_I4B, 0.0_DFP)) .EQ. DFP, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_27(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_27

  MODULE PURE FUNCTION Zeros_28(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_28
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                                       Zeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-29
! summary: Returns a matrix of zeros whose shape is given by a vector.
!
! Description:
!   Returns a matrix of shape `(s(1), s(2))` whose entries are all
!   zero. This is handy to make a zero matrix with the shape of another
!   matrix, `Zeros(SHAPE(b), datatype)`. It is available for `INT8`,
!   `INT16`, `INT32`, `INT64`, `REAL32` and `REAL64`.
!
!   - `datatype` only selects the type and kind of the result. Its
!     value is not used.
!   - Only `s(1)` and `s(2)` are used. `SIZE(s) >= 2` is not checked.
!
! Arguments:
!   s : INTEGER(I4B), rank(1) - shape of the result, `SIZE(s) >= 2`
!   datatype : INTEGER or REAL - any value of the wanted type and kind
!
! Returns:
!   ans : same type and kind as `datatype`, rank(2) - shape
!     `(s(1), s(2))`, all entries zero
!
! Formula:
!   $$\mathrm{ans}_{i,j} = 0, \quad i \le s_1, \ j \le s_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ZerosUtility, ONLY: Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: ia(:, :)
! REAL(DFP), ALLOCATABLE :: ra(:, :)
! REAL(DFP) :: b(4, 2)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! integer matrix of shape s = [2, 3]
! ia = Zeros([2_I4B, 3_I4B], 1_I4B)
! CALL OK(ALL(SHAPE(ia) .EQ. [2_I4B, 3_I4B]), "shape (integer)")
! CALL OK(ALL(ia .EQ. 0_I4B), "all entries are zero (integer)")
!
! ! real matrix with the same shape as another matrix
! b = 1.0_DFP
! ra = Zeros(SHAPE(b), 1.0_DFP)
! CALL OK(ALL(SHAPE(ra) .EQ. SHAPE(b)), "shape is SHAPE(b)")
! CALL OK(ALL(ABS(ra) .LT. tol), "all entries are zero (real)")
! CALL OK(KIND(ra) .EQ. DFP, "kind of the result")
!
! ! zero size in one dimension
! ra = Zeros([0_I4B, 3_I4B], 1.0_DFP)
! CALL OK(SIZE(ra) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_29_Int8(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int8

  MODULE PURE FUNCTION Zeros_29_Int16(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int16

  MODULE PURE FUNCTION Zeros_29_Int32(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int32

  MODULE PURE FUNCTION Zeros_29_Int64(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int64

  MODULE PURE FUNCTION Zeros_29_Real32(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Real32

  MODULE PURE FUNCTION Zeros_29_Real64(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Real64
END INTERFACE Zeros

END MODULE ZerosUtility
