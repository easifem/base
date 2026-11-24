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
! summary: Constructors of nodal variables (`NodalVariable`)
!
! The generic `NodalVariable` returns a `FEVariable_` defined on nodes.
! A nodal variable holds one value, vector or matrix per node.
!
! Two type arguments select the specific procedure.
! The first is the rank: `TypeFEVariableScalar`, `TypeFEVariableVector`
! or `TypeFEVariableMatrix`.
! The second is the variable type: `TypeFEVariableConstant`,
! `TypeFEVariableSpace`, `TypeFEVariableTime` or
! `TypeFEVariableSpaceTime`.
!
! Each combination has up to three constructors.
! One takes an array of values. One takes flat values and a shape.
! One takes only the sizes and sets all values to zero.
! The variable keeps its values in column major order.
! The shape `s` of each combination is:
!
! - scalar, constant: [1]
! - scalar, space: [nns]
! - scalar, time: [nnt]
! - scalar, space-time: [nns, nnt]
! - vector, constant: [ncomp]
! - vector, space: [ncomp, nns]
! - vector, time: [ncomp, nnt]
! - vector, space-time: [ncomp, nns, nnt]
! - matrix, constant: [nrow, ncol]
! - matrix, space: [nrow, ncol, nns]
! - matrix, time: [nrow, ncol, nnt]
! - matrix, space-time: [nrow, ncol, nns, nnt]
!
! Here `nns` and `nnt` are the number of nodes in space and in time.
! The vector length is `ncomp`.
! The matrix has `nrow` rows and `ncol` columns.

MODULE FEVariable_NodalVariableMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE
PRIVATE

PUBLIC :: NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, constant, from values
!
! Returns a nodal variable that holds one constant scalar value.
!
! The result has shape `s = [1]` and length `1`. Its only entry is `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val
    !! constant value
    CLASS(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    CLASS(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
  END FUNCTION Nodal_Scalar_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space, from values
!
! Returns a scalar nodal variable that varies in space.
! The array `val` holds one value for each node.
!
! The shape is `s = [nns]`, where `nns` is `SIZE(val)`.
! The length is `nns`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, `SIZE(val) = nns`
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Scalar_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-04
! summary: Create nodal variable, scalar, space, with zero values
!
! Returns a scalar nodal variable that varies in space.
! All values are zero.
!
! The shape is `s = [tsize]` and the length is `tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   zero nodes and with reassignment

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Space2(tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of nodes in space
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Scalar_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, time, from values
!
! Returns a scalar nodal variable that varies in time.
! The array `val` holds one value for each time node.
!
! The shape is `s = [nnt]`, where `nnt` is `SIZE(val)`.
! The length is `nnt`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, `SIZE(val) = nnt`
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Scalar_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, time, with zero values
!
! Returns a scalar nodal variable that varies in time.
! All values are zero.
!
! The shape is `s = [tsize]` and the length is `tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   reassignment

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Time2(tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of nodes in time
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Scalar_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, from values
!
! Returns a scalar nodal variable that varies in space and time.
! The entry `val(i, j)` is the value at space node `i` and time node `j`.
!
! The shape is `s = [nns, nnt]`, taken from `SHAPE(val)`.
! The length is `nns * nnt`.
! The variable keeps the values column by column.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, `SHAPE(val) = [nns, nnt]`
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Scalar_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, from flat values
!
! Returns a scalar nodal variable that varies in space and time.
! The values come from a flat array and the shape comes from `s`.
!
! The length is `s(1) * s(2)`.
! The code reads the first `s(1) * s(2)` entries of `val` in column major
! order. The space index runs fastest.
!
! @note
! `SIZE(val)` must be at least `s(1) * s(2)`.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with a
!   longer `val`

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of the data, `[nns, nnt]`
  END FUNCTION Nodal_Scalar_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, with zero values
!
! Returns a scalar nodal variable that varies in space and time.
! All values are zero.
!
! The shape is `s = [nrow, ncol]` and the length is `nrow * ncol`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the zero values

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of nodes in time
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! use `TypeFEVariableScalar`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Scalar_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, constant, from values
!
! Returns a constant vector nodal variable.
! The array `val` holds one value for each component.
!
! The shape is `s = [ncomp]`, where `ncomp` is `SIZE(val)`.
! The length is `ncomp`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! component values, `SIZE(val) = ncomp`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
  END FUNCTION Nodal_Vector_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, constant, with zero values
!
! Returns a constant vector nodal variable.
! All values are zero.
!
! The shape is `s = [tsize]` and the length is `tsize`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the zero values

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Constant2( &
    tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of components
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
  END FUNCTION Nodal_Vector_Constant2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space, from values
!
! Returns a vector nodal variable that varies in space.
! The entry `val(i, j)` is component `i` at space node `j`.
!
! The shape is `s = [ncomp, nns]`, taken from `SHAPE(val)`.
! The length is `ncomp * nns`.
! The variable keeps the values column by column.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the value order

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, `SHAPE(val) = [ncomp, nns]`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Vector_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space, from flat values
!
! Returns a vector nodal variable that varies in space.
! The values come from a flat array and the shape comes from `s`.
!
! The length is `s(1) * s(2)`.
! The code reads the first `s(1) * s(2)` entries of `val` in column major
! order. The component index runs fastest.
!
! @note
! `SIZE(val)` must be at least `s(1) * s(2)`.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the value order

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of the data, `[ncomp, nns]`
  END FUNCTION Nodal_Vector_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Create nodal variable, vector, space, with zero values
!
! Returns a vector nodal variable that varies in space.
! All values are zero.
!
! The shape is `s = [nrow, ncol]` and the length is `nrow * ncol`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the zero values

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of components
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of nodes in space
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Vector_Space3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, from values
!
! Returns a vector nodal variable that varies in time.
! The entry `val(i, j)` is component `i` at time node `j`.
!
! The shape is `s = [ncomp, nnt]`, taken from `SHAPE(val)`.
! The length is `ncomp * nnt`.
! The variable keeps the values column by column.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, `SHAPE(val) = [ncomp, nnt]`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Vector_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, from flat values
!
! Returns a vector nodal variable that varies in time.
! The values come from a flat array and the shape comes from `s`.
!
! The length is `s(1) * s(2)`.
! The code reads the first `s(1) * s(2)` entries of `val` in column major
! order. The component index runs fastest.
!
! @note
! `SIZE(val)` must be at least `s(1) * s(2)`.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the value order

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of the data, `[ncomp, nnt]`
  END FUNCTION Nodal_Vector_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, with zero values
!
! Returns a vector nodal variable that varies in time.
! All values are zero.
!
! The shape is `s = [nrow, ncol]` and the length is `nrow * ncol`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the zero values

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of components
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of nodes in time
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Vector_Time3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, from values
!
! Returns a vector nodal variable that varies in space and time.
! The entry `val(i, j, k)` is component `i` at space node `j` and time
! node `k`.
!
! The shape is `s = [ncomp, nns, nnt]`, taken from `SHAPE(val)`.
! The length is `ncomp * nns * nnt`.
! The variable keeps the values in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, `SHAPE(val) = [ncomp, nns, nnt]`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Vector_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, from flat values
!
! Returns a vector nodal variable that varies in space and time.
! The values come from a flat array and the shape comes from `s`.
!
! The length is `s(1) * s(2) * s(3)`.
! The code reads the first `s(1) * s(2) * s(3)` entries of `val` in column
! major order. The component index runs fastest.
!
! @note
! `SIZE(val)` must be at least `s(1) * s(2) * s(3)`.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the value order

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of the data, `[ncomp, nns, nnt]`
  END FUNCTION Nodal_Vector_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, with zero values
!
! Returns a vector nodal variable that varies in space and time.
! All values are zero.
!
! The shape is `s = [dim1, dim2, dim3]` and the length is
! `dim1 * dim2 * dim3`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90) with
!   check of the zero values

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of components
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of nodes in time
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! use `TypeFEVariableVector`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Vector_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, from values
!
! Returns a constant matrix nodal variable.
! The array `val` holds the matrix.
!
! The shape is `s = [nrow, ncol]`, taken from `SHAPE(val)`.
! The length is `nrow * ncol`.
! The variable keeps the values column by column.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! matrix values, `SHAPE(val) = [nrow, ncol]`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
  END FUNCTION Nodal_Matrix_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, from flat values
!
! Returns a constant matrix nodal variable.
! The array `val` holds the matrix values in a flat list.
!
! The shape is `s = [nrow, ncol]`.
! The length is `PRODUCT(s)`.
! The function reads only the first `PRODUCT(s)` entries of `val`.
! They must be in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! matrix values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of the matrix, `[nrow, ncol]`
  END FUNCTION Nodal_Matrix_Constant2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, with zero values
!
! Returns a constant matrix nodal variable with all values zero.
!
! The shape is `s = [nrow, ncol]`.
! The length is `nrow * ncol`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant3( &
    nrow, ncol, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! use `TypeFEVariableConstant`
  END FUNCTION Nodal_Matrix_Constant3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, from values
!
! Returns a matrix nodal variable that varies in space.
! The array `val` holds one matrix for each space node.
!
! The shape is `s = [nrow, ncol, nns]`, taken from `SHAPE(val)`.
! Here `nns` is the number of nodes in space.
! The length is `PRODUCT(s)`.
! The variable keeps the values in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! matrix values, `SHAPE(val) = [nrow, ncol, nns]`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Matrix_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, from flat values
!
! Returns a matrix nodal variable that varies in space.
! The array `val` holds the values in a flat list.
!
! The shape is `s = [nrow, ncol, nns]`.
! Here `nns` is the number of nodes in space.
! The length is `PRODUCT(s)`.
! The function reads only the first `PRODUCT(s)` entries of `val`.
! They must be in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! matrix values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of the data, `[nrow, ncol, nns]`
  END FUNCTION Nodal_Matrix_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, with zero values
!
! Returns a matrix nodal variable that varies in space.
! All values are zero.
!
! The shape is `s = [dim1, dim2, dim3]`.
! The length is `PRODUCT(s)`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of nodes in space
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpace`
  END FUNCTION Nodal_Matrix_Space3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, from values
!
! Returns a matrix nodal variable that varies in time.
! The array `val` holds one matrix for each time node.
!
! The shape is `s = [nrow, ncol, nnt]`, taken from `SHAPE(val)`.
! Here `nnt` is the number of nodes in time.
! The length is `PRODUCT(s)`.
! The variable keeps the values in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! matrix values, `SHAPE(val) = [nrow, ncol, nnt]`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Matrix_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, from flat values
!
! Returns a matrix nodal variable that varies in time.
! The array `val` holds the values in a flat list.
!
! The shape is `s = [nrow, ncol, nnt]`.
! Here `nnt` is the number of nodes in time.
! The length is `PRODUCT(s)`.
! The function reads only the first `PRODUCT(s)` entries of `val`.
! They must be in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! matrix values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of the data, `[nrow, ncol, nnt]`
  END FUNCTION Nodal_Matrix_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, with zero values
!
! Returns a matrix nodal variable that varies in time.
! All values are zero.
!
! The shape is `s = [dim1, dim2, dim3]`.
! The length is `PRODUCT(s)`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
! - [test 2](|base|FEVariable/examples/NodalVariable_test_2.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time3( &
    dim1, dim2, dim3, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of nodes in time
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableTime`
  END FUNCTION Nodal_Matrix_Time3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, from values
!
! Returns a matrix nodal variable that varies in space and time.
! The array `val` holds one matrix for each space and time node.
!
! The shape is `s = [nrow, ncol, nns, nnt]`, taken from `SHAPE(val)`.
! Here `nns` is the number of nodes in space.
! And `nnt` is the number of nodes in time.
! The length is `PRODUCT(s)`.
! The variable keeps the values in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! matrix values, `SHAPE(val) = [nrow, ncol, nns, nnt]`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Matrix_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, from flat values
!
! Returns a matrix nodal variable that varies in space and time.
! The array `val` holds the values in a flat list.
!
! The shape is `s = [nrow, ncol, nns, nnt]`.
! Here `nns` is the number of nodes in space.
! And `nnt` is the number of nodes in time.
! The length is `PRODUCT(s)`.
! The function reads only the first `PRODUCT(s)` entries of `val`.
! They must be in column major order.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! matrix values in column major order, `SIZE(val) >= PRODUCT(s)`
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
    INTEGER(I4B), INTENT(IN) :: s(4)
    !! shape of the data, `[nrow, ncol, nns, nnt]`
  END FUNCTION Nodal_Matrix_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, with zero values
!
! Returns a matrix nodal variable that varies in space and time.
! All values are zero.
!
! The shape is `s = [dim1, dim2, dim3, dim4]`.
! This is `[nrow, ncol, nns, nnt]`.
! Here `nns` is the number of nodes in space.
! And `nnt` is the number of nodes in time.
! The length is `PRODUCT(s)`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/NodalVariable_test_1.F90)
!
! See also: [[NodalVariable]]

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime3( &
    dim1, dim2, dim3, dim4, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: dim1
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: dim2
    !! number of columns
    INTEGER(I4B), INTENT(IN) :: dim3
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: dim4
    !! number of nodes in time
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! use `TypeFEVariableMatrix`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! use `TypeFEVariableSpaceTime`
  END FUNCTION Nodal_Matrix_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_NodalVariableMethod
