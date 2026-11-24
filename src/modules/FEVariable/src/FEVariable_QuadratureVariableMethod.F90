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

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Constructors of quadrature variables (QuadratureVariable)
!
! `QuadratureVariable` returns a `FEVariable_` defined on quadrature
! points.
! Two type arguments select the specific procedure.
! The `rank` argument is `TypeFEVariableScalar`, `TypeFEVariableVector`
! or `TypeFEVariableMatrix`.
! The `vartype` argument is `TypeFEVariableConstant`,
! `TypeFEVariableSpace`, `TypeFEVariableTime` or
! `TypeFEVariableSpaceTime`.
! The variable keeps its values in `val`, in column-major order.
! The shape `s` of each combination is:
!
! - scalar, constant: [1]
! - scalar, space: [nips]
! - scalar, time: [nipt]
! - scalar, space-time: [nips, nipt]
! - vector, constant: [ncomp]
! - vector, space: [ncomp, nips]
! - vector, time: [ncomp, nipt]
! - vector, space-time: [ncomp, nips, nipt]
! - matrix, constant: [nrow, ncol]
! - matrix, space: [nrow, ncol, nips]
! - matrix, time: [nrow, ncol, nipt]
! - matrix, space-time: [nrow, ncol, nips, nipt]
!
! Here `nips` and `nipt` are the numbers of quadrature points in space
! and in time.

MODULE FEVariable_QuadratureVariableMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE BaseType, ONLY: FEVariableConstant_
USE BaseType, ONLY: FEVariableSpace_
USE BaseType, ONLY: FEVariableTime_
USE BaseType, ONLY: FEVariableSpaceTime_
USE BaseType, ONLY: TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE

PUBLIC :: QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, constant, from values
!
! Creates a scalar quadrature variable that has one constant value.
! The same value applies at every quadrature point.
!
! The result has `s = [1]` and `len = 1`. Its only value is `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Constant( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val
    !! constant value
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Quadrature_Scalar_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, space, from values
!
! Creates a scalar quadrature variable that varies in space.
! It holds one value for each space quadrature point.
!
! The result has `s = [nips]` with `nips = SIZE(val)`, and `len = nips`.
! It stores a copy of `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nips]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Scalar_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-04
! summary: Create quadrature variable, scalar, space, with zero values
!
! Creates a scalar quadrature variable that varies in space.
! All values are zero.
!
! The result has `s = [tsize]` and `len = tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Space2( &
    tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of quadrature points in space
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Scalar_Space2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, time, from values
!
! Creates a scalar quadrature variable that varies in time.
! It holds one value for each time quadrature point.
!
! The result has `s = [nipt]` with `nipt = SIZE(val)`, and `len = nipt`.
! It stores a copy of `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nipt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Scalar_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, time, with zero values
!
! Creates a scalar quadrature variable that varies in time.
! All values are zero.
!
! The result has `s = [tsize]` and `len = tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Time2( &
    tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of quadrature points in time
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Scalar_Time2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, space-time, from values
!
! Creates a scalar quadrature variable that varies in space and time.
! It holds one value for each pair of space and time quadrature points.
!
! The result has `s = [nips, nipt]` equal to `SHAPE(val)`, and
! `len = nips * nipt`. The result keeps the values of `val` column by column.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nips, nipt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Scalar_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, space-time, from flat values
!
! Creates a scalar quadrature variable that varies in space and time.
! The values come from a flat array, and `s` gives the shape.
!
! The result has `len = s(1) * s(2)`. It uses the first `len` entries of
! `val`, in column major order. It skips any extra entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) with
!   `val` longer than `PRODUCT(s)`

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [nips, nipt]
  END FUNCTION Quadrature_Scalar_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, scalar, space-time, with zero values
!
! Creates a scalar quadrature variable that varies in space and time.
! All values are zero.
!
! The result has `s = [nrow, ncol]` and `len = nrow * ncol`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of quadrature points in space
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of quadrature points in time
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Scalar_SpaceTime3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, constant, from values
!
! Creates a vector quadrature variable that is constant in space and time.
! The same vector applies at every quadrature point.
!
! The result has `s = [ncomp]` with `ncomp = SIZE(val)`, and `len = ncomp`.
! It stores a copy of `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Constant( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [ncomp]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Quadrature_Vector_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, constant, with zero values
!
! Creates a vector quadrature variable that is constant in space and time.
! All components are zero.
!
! The result has `s = [tsize]` and `len = tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Constant2( &
    tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of components
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Quadrature_Vector_Constant2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, space, from values
!
! Creates a vector quadrature variable that varies in space.
! The values come from the matrix `val`.
!
! The shape of the variable is `SHAPE(val) = [ncomp, nips]`.
! Here `ncomp` is the number of components and `nips` is the number of
! quadrature points in space. The variable holds `ncomp * nips` values.
! The routine copies them from `val` in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
!
! See also: [[Quadrature_Vector_Space2]], [[Quadrature_Vector_Space3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nips]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Vector_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, space, from flat values
!
! Creates a vector quadrature variable that varies in space.
! The values come from the flat array `val`.
!
! The shape `s = [ncomp, nips]` gives the number of components and the
! number of quadrature points in space. The variable copies the first
! `PRODUCT(s)` entries of `val`, read in column major order. It ignores
! any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the values
!
! See also: [[Quadrature_Vector_Space]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Space2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [ncomp, nips]
  END FUNCTION Quadrature_Vector_Space2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Create quadrature variable, vector, space, with zero values
!
! Creates a vector quadrature variable that varies in space.
! All values are zero.
!
! The shape is `[nrow, ncol]`. Here `nrow` is the number of components
! and `ncol` is the number of quadrature points in space.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the zero values
!
! See also: [[Quadrature_Vector_Space]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Space3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of components
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of quadrature points in space
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Vector_Space3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, time, from values
!
! Creates a vector quadrature variable that varies in time.
! The values come from the matrix `val`.
!
! The shape of the variable is `SHAPE(val) = [ncomp, nipt]`.
! Here `ncomp` is the number of components and `nipt` is the number of
! quadrature points in time. The variable holds `ncomp * nipt` values.
! The routine copies them from `val` in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
!
! See also: [[Quadrature_Vector_Time2]], [[Quadrature_Vector_Time3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nipt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Vector_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, time, from flat values
!
! Creates a vector quadrature variable that varies in time.
! The values come from the flat array `val`.
!
! The shape `s = [ncomp, nipt]` gives the number of components and the
! number of quadrature points in time. The variable copies the first
! `PRODUCT(s)` entries of `val`, read in column major order. It ignores
! any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the values
!
! See also: [[Quadrature_Vector_Time]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Time2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [ncomp, nipt]
  END FUNCTION Quadrature_Vector_Time2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Create quadrature variable, vector, time, with zero values
!
! Creates a vector quadrature variable that varies in time.
! All values are zero.
!
! The shape is `[nrow, ncol]`. Here `nrow` is the number of components
! and `ncol` is the number of quadrature points in time.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the zero values
!
! See also: [[Quadrature_Vector_Time]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Time3(nrow, ncol, rank, vartype) &
    RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of components
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of quadrature points in time
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Vector_Time3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, space-time, from values
!
! Creates a vector quadrature variable that varies in space and time.
! The values come from the rank-3 array `val`.
!
! The shape of the variable is `SHAPE(val) = [ncomp, nips, nipt]`.
! Here `ncomp` is the number of components. The terms `nips` and `nipt`
! are the numbers of quadrature points in space and in time. The variable
! holds `ncomp * nips * nipt` values. The routine copies them from `val`
! in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
!
! See also: [[Quadrature_Vector_SpaceTime2]],
! [[Quadrature_Vector_SpaceTime3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [ncomp, nips, nipt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Vector_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, vector, space-time, from flat values
!
! Creates a vector quadrature variable that varies in space and time.
! The values come from the flat array `val`.
!
! The shape `s = [ncomp, nips, nipt]` gives the number of components and
! the numbers of quadrature points in space and in time. The variable
! copies the first `PRODUCT(s)` entries of `val`, read in column major
! order. It ignores any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the values
!
! See also: [[Quadrature_Vector_SpaceTime]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [ncomp, nips, nipt]
  END FUNCTION Quadrature_Vector_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-11
! summary: Create quadrature variable, vector, space-time, with zero values
!
! Creates a vector quadrature variable that varies in space and time.
! All values are zero.
!
! The shape is `[dim1, dim2, dim3]`. Here `dim1` is the number of
! components. The terms `dim2` and `dim3` are the numbers of quadrature
! points in space and in time.
!
! Changes:
!   - 2026-11-14: dim1, dim2, dim3 moved before rank and vartype, as in
!     the other size-only constructors
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks the zero values
!
! See also: [[Quadrature_Vector_SpaceTime]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of components
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of quadrature points in space
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of quadrature points in time
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Vector_SpaceTime3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, constant, from values
!
! Creates a matrix quadrature variable that is constant in space and time.
! The values come from the matrix `val`.
!
! The shape of the variable is `SHAPE(val) = [nrow, ncol]`. It holds
! `nrow * ncol` values. The routine copies them from `val` in column
! major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
!
! See also: [[Quadrature_Matrix_Constant2]], [[Quadrature_Matrix_Constant3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Constant( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nrow, ncol]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Quadrature_Matrix_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, constant, from flat values
!
! Creates a matrix quadrature variable that is constant in space and time.
! The values come from the flat array `val`.
!
! The shape `s = [nrow, ncol]` gives the matrix size. The variable holds
! `PRODUCT(s)` values. The routine copies the first `PRODUCT(s)` entries
! of `val`, in column major order. It ignores any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the copied values
!
! See also: [[Quadrature_Matrix_Constant]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Constant2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [nrow, ncol]
  END FUNCTION Quadrature_Matrix_Constant2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, constant, with zero values
!
! Creates a matrix quadrature variable that is constant in space and time.
! Every value is zero.
!
! The arguments `nrow` and `ncol` give the matrix size. The variable holds
! `nrow * ncol` values.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the zero values
!
! See also: [[Quadrature_Matrix_Constant]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Constant3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Quadrature_Matrix_Constant3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space, from values
!
! Creates a matrix quadrature variable that varies in space.
! It holds one matrix at each space quadrature point.
!
! The shape is `SHAPE(val) = [nrow, ncol, nips]`. Here `nips` is the
! number of quadrature points in space. The variable holds
! `nrow * ncol * nips` values. The routine copies them from `val` in
! column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)
!
! See also: [[Quadrature_Matrix_Space2]], [[Quadrature_Matrix_Space3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nips]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Matrix_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space, from flat values
!
! Creates a matrix quadrature variable that varies in space.
! The values come from the flat array `val`.
!
! The shape `s = [nrow, ncol, nips]` gives the matrix size and the number
! of space quadrature points. The variable holds `PRODUCT(s)` values. The
! routine copies the first `PRODUCT(s)` entries of `val`, in column major
! order. It ignores any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the copied values
!
! See also: [[Quadrature_Matrix_Space]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Space2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [nrow, ncol, nips]
  END FUNCTION Quadrature_Matrix_Space2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space, with zero values
!
! Creates a matrix quadrature variable that varies in space.
! Every value is zero.
!
! The arguments `dim1`, `dim2` and `dim3` give the number of rows, the
! number of columns and the number of space quadrature points. The variable
! holds `dim1 * dim2 * dim3` values.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the zero values
!
! See also: [[Quadrature_Matrix_Space]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Space3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of quadrature points in space
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Quadrature_Matrix_Space3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, time, from values
!
! Creates a matrix quadrature variable that varies in time.
! It holds one matrix at each time quadrature point.
!
! The shape is `SHAPE(val) = [nrow, ncol, nipt]`. Here `nipt` is the
! number of quadrature points in time. The variable holds
! `nrow * ncol * nipt` values. The routine copies them from `val` in
! column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)
!
! See also: [[Quadrature_Matrix_Time2]], [[Quadrature_Matrix_Time3]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nipt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Matrix_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, time, from flat values
!
! Creates a matrix quadrature variable that varies in time.
! The values come from the flat array `val`.
!
! The shape `s = [nrow, ncol, nipt]` gives the matrix size and the number
! of time quadrature points. The variable holds `PRODUCT(s)` values. The
! routine copies the first `PRODUCT(s)` entries of `val`, in column major
! order. It ignores any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the copied values
!
! See also: [[Quadrature_Matrix_Time]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Time2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [nrow, ncol, nipt]
  END FUNCTION Quadrature_Matrix_Time2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, time, with zero values
!
! Creates a matrix quadrature variable that varies in time.
! Every value is zero.
!
! The arguments `dim1`, `dim2` and `dim3` give the number of rows, the
! number of columns and the number of time quadrature points. The variable
! holds `dim1 * dim2 * dim3` values.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the zero values
!
! See also: [[Quadrature_Matrix_Time]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Time3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of quadrature points in time
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Quadrature_Matrix_Time3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space-time, from values
!
! Creates a matrix quadrature variable that varies in space and time.
! It holds one matrix at each pair of space and time quadrature points.
!
! The shape is `SHAPE(val) = [nrow, ncol, nips, nipt]`. Here `nips` and
! `nipt` are the numbers of quadrature points in space and in time. The
! variable holds `nrow * ncol * nips * nipt` values. The routine copies
! them from `val` in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)
!
! See also: [[Quadrature_Matrix_SpaceTime2]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime( &
    val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nips, nipt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Matrix_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space-time, from flat values
!
! Creates a matrix quadrature variable that varies in space and time.
! The values come from the flat array `val`.
!
! The shape `s = [nrow, ncol, nips, nipt]` gives the matrix size and the
! numbers of quadrature points in space and in time. The variable holds
! `PRODUCT(s)` values. The routine copies the first `PRODUCT(s)` entries
! of `val`, in column major order. It ignores any further entries.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90) checks
!   the copied values
!
! See also: [[Quadrature_Matrix_SpaceTime]]

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(4)
    !! shape of data, [nrow, ncol, nips, nipt]
  END FUNCTION Quadrature_Matrix_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                       QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create quadrature variable, matrix, space-time, with zero values
!
! Creates a matrix quadrature variable that varies in space and time.
! All values start at zero.
!
! The shape of the variable is `[dim1, dim2, dim3, dim4]`.
! These are the rows, the columns, the quadrature points in space and
! the quadrature points in time.
! The total length is the product of the four sizes.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/QuadratureVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/QuadratureVariable_test_2.F90)

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime3( &
    dim1, dim2, dim3, dim4, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! quadrature variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of quadrature points in space
    INTEGER(I4B), INTENT(IN) :: dim4
    !! number of quadrature points in time
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Quadrature_Matrix_SpaceTime3
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_QuadratureVariableMethod
