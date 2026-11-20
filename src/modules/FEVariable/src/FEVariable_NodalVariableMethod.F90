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
! summary: Constructors of nodal variables (NodalVariable)
!
! `NodalVariable` returns a FEVariable_ with `defineOn` set to
! nodal.
! The rank (TypeFEVariableScalar, TypeFEVariableVector,
! TypeFEVariableMatrix) and the variable type
! (TypeFEVariableConstant, TypeFEVariableSpace, TypeFEVariableTime,
! TypeFEVariableSpaceTime) are passed as type arguments and select
! the specific procedure. Values are stored column major in `val`.
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
! where nns and nnt are the number of nodes in space and time

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
! Create a scalar, constant nodal variable with `s = [1]`, `len = 1` and
! `val(1) = val`.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableScalar, &
!                     TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val
    !! constant value
    CLASS(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    CLASS(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Nodal_Scalar_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space, from values
!
! Create a scalar, space nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nns]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableScalar, &
!                     TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nns]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Scalar_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-04
! summary: Create nodal variable, scalar, space, with zero values
!
! Create a scalar, space nodal variable with shape `s = [nns]` given by the
! size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(tsize=2, &
!                     rank=TypeFEVariableScalar, &
!                     vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Space2(tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of nodes in space
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Scalar_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, time, from values
!
! Create a scalar, time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nnt]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableScalar, &
!                     TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nnt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Scalar_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, time, with zero values
!
! Create a scalar, time nodal variable with shape `s = [nnt]` given by the
! size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(tsize=2, &
!                     rank=TypeFEVariableScalar, &
!                     vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Time2(tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of nodes in time
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Scalar_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, from values
!
! Create a scalar, space-time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nns, nnt]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableScalar, &
!                     TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nns, nnt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Scalar_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, from flat values
!
! Create a scalar, space-time nodal variable with shape `s = [nns, nnt]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableScalar, &
!                     TypeFEVariableSpaceTime, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [nns, nnt]
  END FUNCTION Nodal_Scalar_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, scalar, space-time, with zero values
!
! Create a scalar, space-time nodal variable with shape `s = [nns, nnt]` given
! by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(nrow=2, ncol=2, &
!                     rank=TypeFEVariableScalar, &
!                     vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Scalar_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, constant, from values
!
! Create a vector, constant nodal variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [ncomp]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Nodal_Vector_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, constant, with zero values
!
! Create a vector, constant nodal variable with shape `s = [ncomp]` given by
! the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(tsize=2, &
!                     rank=TypeFEVariableVector, &
!                     vartype=TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Constant2( &
    tsize, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! number of components
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Nodal_Vector_Constant2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space, from values
!
! Create a vector, space nodal variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nns]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nns]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Vector_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space, from flat values
!
! Create a vector, space nodal variable with shape `s = [ncomp, nns]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableSpace, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [ncomp, nns]
  END FUNCTION Nodal_Vector_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Create nodal variable, vector, space, with zero values
!
! Create a vector, space nodal variable with shape `s = [ncomp, nns]` given by
! the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(nrow=2, ncol=2, &
!                     rank=TypeFEVariableVector, &
!                     vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Vector_Space3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, from values
!
! Create a vector, time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nnt]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nnt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Vector_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, from flat values
!
! Create a vector, time nodal variable with shape `s = [ncomp, nnt]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableTime, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [ncomp, nnt]
  END FUNCTION Nodal_Vector_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, time, with zero values
!
! Create a vector, time nodal variable with shape `s = [ncomp, nnt]` given by
! the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(nrow=2, ncol=2, &
!                     rank=TypeFEVariableVector, &
!                     vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Vector_Time3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, from values
!
! Create a vector, space-time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nns, nnt]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [ncomp, nns, nnt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Vector_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, from flat values
!
! Create a vector, space-time nodal variable with shape
! `s = [ncomp, nns, nnt]` and `len = PRODUCT(s)`. The data `val(1:len)` must
! be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableVector, &
!                     TypeFEVariableSpaceTime, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [ncomp, nns, nnt]
  END FUNCTION Nodal_Vector_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, vector, space-time, with zero values
!
! Create a vector, space-time nodal variable with shape
! `s = [ncomp, nns, nnt]` given by the size arguments, `len = PRODUCT(s)` and
! all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(dim1=2, dim2=2, dim3=2, &
!                     rank=TypeFEVariableVector, &
!                     vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Vector_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, from values
!
! Create a matrix, constant nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nrow, ncol]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Nodal_Matrix_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, from flat values
!
! Create a matrix, constant nodal variable with shape `s = [nrow, ncol]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableConstant, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    INTEGER(I4B), INTENT(IN) :: s(2)
    !! shape of data, [nrow, ncol]
  END FUNCTION Nodal_Matrix_Constant2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, constant, with zero values
!
! Create a matrix, constant nodal variable with shape `s = [nrow, ncol]` given
! by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(nrow=2, ncol=2, &
!                     rank=TypeFEVariableMatrix, &
!                     vartype=TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
  END FUNCTION Nodal_Matrix_Constant3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, from values
!
! Create a matrix, space nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nns]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Matrix_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, from flat values
!
! Create a matrix, space nodal variable with shape `s = [nrow, ncol, nns]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableSpace, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [nrow, ncol, nns]
  END FUNCTION Nodal_Matrix_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space, with zero values
!
! Create a matrix, space nodal variable with shape `s = [nrow, ncol, nns]`
! given by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(dim1=2, dim2=2, dim3=2, &
!                     rank=TypeFEVariableMatrix, &
!                     vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
  END FUNCTION Nodal_Matrix_Space3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, from values
!
! Create a matrix, time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nnt]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nnt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Matrix_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, from flat values
!
! Create a matrix, time nodal variable with shape `s = [nrow, ncol, nnt]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableTime, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    INTEGER(I4B), INTENT(IN) :: s(3)
    !! shape of data, [nrow, ncol, nnt]
  END FUNCTION Nodal_Matrix_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, time, with zero values
!
! Create a matrix, time nodal variable with shape `s = [nrow, ncol, nnt]`
! given by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(dim1=2, dim2=2, dim3=2, &
!                     rank=TypeFEVariableMatrix, &
!                     vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
  END FUNCTION Nodal_Matrix_Time3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, from values
!
! Create a matrix, space-time nodal variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nns, nnt]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2, 2)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns, nnt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Matrix_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, from flat values
!
! Create a matrix, space-time nodal variable with shape
! `s = [nrow, ncol, nns, nnt]` and `len = PRODUCT(s)`. The data `val(1:len)`
! must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(16)
! val = 1.0_DFP
! obj = NodalVariable(val, TypeFEVariableMatrix, &
!                     TypeFEVariableSpaceTime, [2, 2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime2( &
    val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    !! nodal variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values in column major order, SIZE(val) >= PRODUCT(s)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    INTEGER(I4B), INTENT(IN) :: s(4)
    !! shape of data, [nrow, ncol, nns, nnt]
  END FUNCTION Nodal_Matrix_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                            NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Create nodal variable, matrix, space-time, with zero values
!
! Create a matrix, space-time nodal variable with shape
! `s = [nrow, ncol, nns, nnt]` given by the size arguments, `len = PRODUCT(s)`
! and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = NodalVariable(dim1=2, dim2=2, dim3=2, dim4=2, &
!                     rank=TypeFEVariableMatrix, &
!                     vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/NodalVariable_test_1.F90

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
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
  END FUNCTION Nodal_Matrix_SpaceTime3
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_NodalVariableMethod
