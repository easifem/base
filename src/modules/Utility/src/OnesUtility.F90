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
! date: 2022-08-05
! summary: Returns arrays with all entries equal to one.
!
! Description:
!   This module contains the generic function `Ones`, which returns an
!   array of rank 1 to 4 with every entry equal to one, similar to
!   `ones` in Matlab or NumPy.
!
!```fortran
! a = Ones(dim1, datatype)
! a = Ones(dim1, dim2, datatype)
! a = Ones(dim1, dim2, dim3, datatype)
! a = Ones(dim1, dim2, dim3, dim4, datatype)
!```
!
!   The last argument `datatype` only selects the type and kind of the
!   result. `Ones` is available for
!
!   - integers (`INT8`, `INT16`, `INT32`, `INT64`, and `INT128` when
!     `USE_Int128` is defined),
!   - reals (`REAL32`, `REAL64`).

MODULE OnesUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Int8
USE GlobalData, ONLY: Int16
USE GlobalData, ONLY: Int32
USE GlobalData, ONLY: Int64
USE GlobalData, ONLY: Real32
USE GlobalData, ONLY: Real64
#ifdef USE_Int128
USE GlobalData, ONLY: Int128
#endif
IMPLICIT NONE
PRIVATE

PUBLIC :: Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns an integer vector with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1)` whose entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i} = 1, \quad i = 1, \dots, \mathrm{dim1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:)
!
! a = Ones(4_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
! CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")
!
! ! only the kind of datatype matters, not its value
! a = Ones(3_I4B, 7_I4B)
! CALL OK(ALL(a .EQ. 1_I4B), "value of datatype is ignored")
! CALL OK(KIND(Ones(3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")
!
! ! zero size
! a = Ones(0_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_1(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int8), INTENT(IN) :: datatype
    INTEGER(Int8) :: ans(dim1)
  END FUNCTION ones_1

  MODULE PURE FUNCTION ones_2(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int16), INTENT(IN) :: datatype
    INTEGER(Int16) :: ans(dim1)
  END FUNCTION ones_2

  MODULE PURE FUNCTION ones_3(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int32), INTENT(IN) :: datatype
    INTEGER(Int32) :: ans(dim1)
  END FUNCTION ones_3

  MODULE PURE FUNCTION ones_4(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int64), INTENT(IN) :: datatype
    INTEGER(Int64) :: ans(dim1)
  END FUNCTION ones_4

#ifdef USE_Int128
  MODULE PURE FUNCTION ones_5(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1)
  END FUNCTION ones_5
#endif
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a real vector with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1)` whose entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i} = 1, \quad i = 1, \dots, \mathrm{dim1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Ones(4_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
! CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")
!
! ! only the kind of datatype matters, not its value
! a = Ones(3_I4B, 0.0_DFP)
! CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "value of datatype is ignored")
! CALL OK(KIND(Ones(3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")
!
! ! zero size
! a = Ones(0_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_6(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(Real32), INTENT(IN) :: datatype
    REAL(Real32) :: ans(dim1)
  END FUNCTION ones_6

  MODULE PURE FUNCTION ones_7(dim1, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(Real64), INTENT(IN) :: datatype
    REAL(Real64) :: ans(dim1)
  END FUNCTION ones_7
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns an integer matrix with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2)` whose entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :)
!
! a = Ones(2_I4B, 3_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
! CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")
! CALL OK(KIND(Ones(2_I4B, 3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")
!
! ! zero size in one dimension
! a = Ones(2_I4B, 0_I4B, 1_I4B)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_8(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int8), INTENT(IN) :: datatype
    INTEGER(Int8) :: ans(dim1, dim2)
  END FUNCTION ones_8

  MODULE PURE FUNCTION ones_9(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int16), INTENT(IN) :: datatype
    INTEGER(Int16) :: ans(dim1, dim2)
  END FUNCTION ones_9

  MODULE PURE FUNCTION ones_10(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int32), INTENT(IN) :: datatype
    INTEGER(Int32) :: ans(dim1, dim2)
  END FUNCTION ones_10

  MODULE PURE FUNCTION ones_11(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int64), INTENT(IN) :: datatype
    INTEGER(Int64) :: ans(dim1, dim2)
  END FUNCTION ones_11

#ifdef USE_Int128
  MODULE PURE FUNCTION ones_12(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2)
  END FUNCTION ones_12
#endif
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a real matrix with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2)` whose entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Ones(2_I4B, 3_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
! CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")
! CALL OK(KIND(Ones(2_I4B, 3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")
!
! ! zero size in one dimension
! a = Ones(0_I4B, 3_I4B, 1.0_DFP)
! CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_13(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(Real32), INTENT(IN) :: datatype
    REAL(Real32) :: ans(dim1, dim2)
  END FUNCTION ones_13

  MODULE PURE FUNCTION ones_14(dim1, dim2, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(Real64), INTENT(IN) :: datatype
    REAL(Real64) :: ans(dim1, dim2)
  END FUNCTION ones_14
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns an integer rank-3 array with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3)` whose entries
!   are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j,k} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :)
!
! a = Ones(2_I4B, 3_I4B, 4_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape of the result")
! CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")
! CALL OK(SUM(a) .EQ. 24_I4B, "sum of entries is the size")
! CALL OK(KIND(Ones(1_I4B, 1_I4B, 1_I4B, 0_I4B)) .EQ. I4B, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_15(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int8), INTENT(IN) :: datatype
    INTEGER(Int8) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_15

  MODULE PURE FUNCTION ones_16(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int16), INTENT(IN) :: datatype
    INTEGER(Int16) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_16

  MODULE PURE FUNCTION ones_17(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int32), INTENT(IN) :: datatype
    INTEGER(Int32) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_17

  MODULE PURE FUNCTION ones_18(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int64), INTENT(IN) :: datatype
    INTEGER(Int64) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_18

#ifdef USE_Int128
  MODULE PURE FUNCTION ones_19(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_19
#endif
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a real rank-3 array with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3)` whose entries
!   are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j,k} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Ones(2_I4B, 3_I4B, 4_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape of the result")
! CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")
! CALL OK(ABS(SUM(a) - 24.0_DFP) .LT. tol, "sum of entries is the size")
! CALL OK(KIND(Ones(1_I4B, 1_I4B, 1_I4B, 0.0_DFP)) .EQ. DFP, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_20(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(Real32), INTENT(IN) :: datatype
    REAL(Real32) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_20

  MODULE PURE FUNCTION ones_21(dim1, dim2, dim3, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(Real64), INTENT(IN) :: datatype
    REAL(Real64) :: ans(dim1, dim2, dim3)
  END FUNCTION ones_21
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns an integer rank-4 array with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3, dim4)` whose
!   entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j,k,l} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :)
!
! a = Ones(2_I4B, 3_I4B, 4_I4B, 5_I4B, 1_I4B)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "shape of the result")
! CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")
! CALL OK(SUM(a) .EQ. 120_I4B, "sum of entries is the size")
! CALL OK(KIND(Ones(1_I4B, 1_I4B, 1_I4B, 1_I4B, 0_I4B)) .EQ. I4B, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_22(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int8), INTENT(IN) :: datatype
    INTEGER(Int8) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_22

  MODULE PURE FUNCTION ones_23(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int16), INTENT(IN) :: datatype
    INTEGER(Int16) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_23

  MODULE PURE FUNCTION ones_24(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int32), INTENT(IN) :: datatype
    INTEGER(Int32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_24

  MODULE PURE FUNCTION ones_25(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int64), INTENT(IN) :: datatype
    INTEGER(Int64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_25

#ifdef USE_Int128
  MODULE PURE FUNCTION ones_26(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_26
#endif
END INTERFACE Ones

!----------------------------------------------------------------------------
!                                                                       Ones
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a real rank-4 array with all entries equal to one.
!
! Description:
!   Returns an array of shape `(dim1, dim2, dim3, dim4)` whose
!   entries are all one.
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
!     `datatype`, all entries one
!
! Formula:
!   $$\mathrm{ans}_{i,j,k,l} = 1 \quad \text{for all indices}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OnesUtility, ONLY: Ones
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! a = Ones(2_I4B, 3_I4B, 4_I4B, 5_I4B, 1.0_DFP)
! CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "shape of the result")
! CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")
! CALL OK(ABS(SUM(a) - 120.0_DFP) .LT. tol, "sum of entries is the size")
! CALL OK(KIND(Ones(1_I4B, 1_I4B, 1_I4B, 1_I4B, 0.0_DFP)) .EQ. DFP, &
!         "kind of the result")
!
! END PROGRAM main
!```

INTERFACE Ones
  MODULE PURE FUNCTION ones_27(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(Real32), INTENT(IN) :: datatype
    REAL(Real32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_27

  MODULE PURE FUNCTION ones_28(dim1, dim2, dim3, dim4, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(Real64), INTENT(IN) :: datatype
    REAL(Real64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION ones_28
END INTERFACE Ones

END MODULE OnesUtility
