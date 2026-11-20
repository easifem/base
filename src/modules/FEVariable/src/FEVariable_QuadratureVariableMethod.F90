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
! `QuadratureVariable` returns a FEVariable_ with `defineOn` set to
! quadrature.
! The rank (TypeFEVariableScalar, TypeFEVariableVector,
! TypeFEVariableMatrix) and the variable type
! (TypeFEVariableConstant, TypeFEVariableSpace, TypeFEVariableTime,
! TypeFEVariableSpaceTime) are passed as type arguments and select
! the specific procedure. Values are stored column major in `val`.
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
! where nips and nipt are the number of quadrature points in space and
! time

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
! Create a scalar, constant quadrature variable with `s = [1]`, `len = 1` and
! `val(1) = val`.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableScalar, &
!                          TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, space quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nips]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableScalar, &
!                          TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, space quadrature variable with shape `s = [nips]` given by
! the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(tsize=2, &
!                          rank=TypeFEVariableScalar, &
!                          vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nipt]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableScalar, &
!                          TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, time quadrature variable with shape `s = [nipt]` given by
! the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(tsize=2, &
!                          rank=TypeFEVariableScalar, &
!                          vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, space-time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nips, nipt]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableScalar, &
!                          TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, space-time quadrature variable with shape
! `s = [nips, nipt]` and `len = PRODUCT(s)`. The data `val(1:len)` must be in
! column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableScalar, &
!                          TypeFEVariableSpaceTime, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a scalar, space-time quadrature variable with shape
! `s = [nips, nipt]` given by the size arguments, `len = PRODUCT(s)` and all
! values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(nrow=2, ncol=2, &
!                          rank=TypeFEVariableScalar, &
!                          vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, constant quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp]`, `len = PRODUCT(s)`, and `val` is stored column
! major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, constant quadrature variable with shape `s = [ncomp]` given
! by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(tsize=2, &
!                          rank=TypeFEVariableVector, &
!                          vartype=TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nips]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space quadrature variable with shape `s = [ncomp, nips]`
! and `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableSpace, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space quadrature variable with shape `s = [ncomp, nips]`
! given by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(nrow=2, ncol=2, &
!                          rank=TypeFEVariableVector, &
!                          vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nipt]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, time quadrature variable with shape `s = [ncomp, nipt]` and
! `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableTime, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, time quadrature variable with shape `s = [ncomp, nipt]`
! given by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(nrow=2, ncol=2, &
!                          rank=TypeFEVariableVector, &
!                          vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space-time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [ncomp, nips, nipt]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space-time quadrature variable with shape
! `s = [ncomp, nips, nipt]` and `len = PRODUCT(s)`. The data `val(1:len)` must
! be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableVector, &
!                          TypeFEVariableSpaceTime, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a vector, space-time quadrature variable with shape
! `s = [ncomp, nips, nipt]` given by the size arguments, `len = PRODUCT(s)`
! and all values zero.
!
! Changes:
!   - 2026-11-14: dim1, dim2, dim3 moved before rank and vartype, as in
!     the other size-only constructors
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(dim1=2, dim2=2, dim3=2, &
!                          rank=TypeFEVariableVector, &
!                          vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, constant quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol]`, `len = PRODUCT(s)`, and `val` is stored
! column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, constant quadrature variable with shape `s = [nrow, ncol]`
! and `len = PRODUCT(s)`. The data `val(1:len)` must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(4)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableConstant, [2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, constant quadrature variable with shape `s = [nrow, ncol]`
! given by the size arguments, `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(nrow=2, ncol=2, &
!                          rank=TypeFEVariableMatrix, &
!                          vartype=TypeFEVariableConstant)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nips]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space quadrature variable with shape
! `s = [nrow, ncol, nips]` and `len = PRODUCT(s)`. The data `val(1:len)` must
! be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableSpace, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space quadrature variable with shape
! `s = [nrow, ncol, nips]` given by the size arguments, `len = PRODUCT(s)` and
! all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(dim1=2, dim2=2, dim3=2, &
!                          rank=TypeFEVariableMatrix, &
!                          vartype=TypeFEVariableSpace)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nipt]`, `len = PRODUCT(s)`, and `val` is
! stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, time quadrature variable with shape
! `s = [nrow, ncol, nipt]` and `len = PRODUCT(s)`. The data `val(1:len)` must
! be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(8)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableTime, [2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, time quadrature variable with shape
! `s = [nrow, ncol, nipt]` given by the size arguments, `len = PRODUCT(s)` and
! all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(dim1=2, dim2=2, dim3=2, &
!                          rank=TypeFEVariableMatrix, &
!                          vartype=TypeFEVariableTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space-time quadrature variable from `val`. The shape is
! `s = SHAPE(val) = [nrow, ncol, nips, nipt]`, `len = PRODUCT(s)`, and `val`
! is stored column major.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2, 2)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space-time quadrature variable with shape
! `s = [nrow, ncol, nips, nipt]` and `len = PRODUCT(s)`. The data `val(1:len)`
! must be in column major order.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(16)
! val = 1.0_DFP
! obj = QuadratureVariable(val, TypeFEVariableMatrix, &
!                          TypeFEVariableSpaceTime, [2, 2, 2, 2])
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
! Create a matrix, space-time quadrature variable with shape
! `s = [nrow, ncol, nips, nipt]` given by the size arguments,
! `len = PRODUCT(s)` and all values zero.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! obj = QuadratureVariable(dim1=2, dim2=2, dim3=2, dim4=2, &
!                          rank=TypeFEVariableMatrix, &
!                          vartype=TypeFEVariableSpaceTime)
!```
!
! Full example: src/modules/FEVariable/examples/QuadratureVariable_test_1.F90

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
