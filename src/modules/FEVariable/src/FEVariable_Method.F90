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

MODULE FEVariable_Method
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

PUBLIC :: Display
PUBLIC :: QuadratureVariable
PUBLIC :: DEALLOCATE
PUBLIC :: NodalVariable
PUBLIC :: SIZE
PUBLIC :: SHAPE
PUBLIC :: OPERATOR(.RANK.)
PUBLIC :: OPERATOR(.vartype.)
PUBLIC :: OPERATOR(.defineon.)
PUBLIC :: isNodalVariable
PUBLIC :: isQuadratureVariable
PUBLIC :: Get
PUBLIC :: Get_
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(*)
PUBLIC :: ABS
PUBLIC :: DOT_PRODUCT
PUBLIC :: OPERATOR(/)
PUBLIC :: OPERATOR(**)
PUBLIC :: SQRT
PUBLIC :: NORM2
PUBLIC :: OPERATOR(.EQ.)
PUBLIC :: OPERATOR(.NE.)
PUBLIC :: MEAN
PUBLIC :: GetLambdaFromYoungsModulus
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: FEVariable_ToChar
PUBLIC :: FEVariable_ToInteger
PUBLIC :: GetInterpolation_

INTEGER(I4B), PARAMETER :: CAPACITY_EXPAND_FACTOR = 1
! capacity = tsize * CAPACITY_EXPAND_FACTOR

!----------------------------------------------------------------------------
!                                 GetLambdaFromYoungsModulus@SpecialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-12
! summary:  Get lame parameter lambda from YoungsModulus

INTERFACE GetLambdaFromYoungsModulus
  MODULE PURE SUBROUTINE fevar_GetLambdaFromYoungsModulus(youngsModulus, &
                                                         shearModulus, lambda)
    TYPE(FEVariable_), INTENT(IN) :: youngsModulus, shearModulus
    TYPE(FEVariable_), INTENT(INOUT) :: lambda
  END SUBROUTINE fevar_GetLambdaFromYoungsModulus
END INTERFACE GetLambdaFromYoungsModulus

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Displays the content of [[FEVariable_]]

INTERFACE Display
  MODULE SUBROUTINE fevar_Display(obj, Msg, UnitNo)
    TYPE(FEVariable_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE fevar_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Constant

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Space

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Time

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Quadrature_Scalar_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Constant

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Space

INTERFACE QuadratureVariable

  MODULE PURE FUNCTION Quadrature_Vector_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Space

INTERFACE QuadratureVariable

  MODULE PURE FUNCTION Quadrature_Vector_Space2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Quadrature_Vector_Space2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Time

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Time

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_Time2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Quadrature_Vector_Time2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Quadrature_Vector_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Constant

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Constant

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Constant2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Quadrature_Matrix_Constant2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Space

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Space

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Space2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Quadrature_Matrix_Space2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Time

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Time

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_Time2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Quadrature_Matrix_Time2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, SpaceTime

INTERFACE QuadratureVariable
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(4)
  END FUNCTION Quadrature_Matrix_SpaceTime2
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Deallocates the content of FEVariable

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE fevar_Deallocate(obj)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
  END SUBROUTINE fevar_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, constant

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariableScalar_), INTENT(IN) :: rank
    CLASS(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, Space

INTERFACE NodalVariable

  MODULE PURE FUNCTION Nodal_Scalar_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, Time

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Nodal_Scalar_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Constant

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Space

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Space

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Nodal_Vector_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Time

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Time

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Nodal_Vector_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime2(val, rank, vartype, s) &
    RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Nodal_Vector_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Constant

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Constant

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Constant2(val, rank, vartype, s) &
    RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(2)
  END FUNCTION Nodal_Matrix_Constant2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Space

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Space

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Space2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Nodal_Matrix_Space2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Time

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Time

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_Time2(val, rank, vartype, s) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(3)
  END FUNCTION Nodal_Matrix_Time2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, SpaceTime

INTERFACE NodalVariable
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime2(val, rank, vartype, s) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    INTEGER(I4B), INTENT(IN) :: s(4)
  END FUNCTION Nodal_Matrix_SpaceTime2
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-13
! summary: obj1 = obj2

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Copy(obj1, obj2)
    TYPE(FEVariable_), INTENT(INOUT) :: obj1
    TYPE(FEVariable_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                              FEVariable_ToChar@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Converts scalar, vector, matrix to string name

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(:), ALLOCATABLE :: ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isUpper
  END FUNCTION FEVariable_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                            FEVariable_ToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Converts scalar, vector, matrix to string name

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToInteger(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION FEVariable_ToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SIZE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the size of variable
!
!# Introduction
!
! If dim is present then obj%s(dim) is returned.
!
! In this case be careful that dim is not out of bound.
!
! Scalar, constant => dim <=1
! Scalar, space or time => dim <=1
! Scalar, spaceTime => dim <=2
!
! Vector, constant => dim <=1
! Vector, space => dim <=2
! Vector, time => dim <=2
! Vector, spaceTime => dim <=3
!
! Matrix, constant => dim <=2
! Matrix, space => dim <=3
! Matrix, time => dim <=3
! Matrix, spaceTime => dim <=4
!
! If dim is absent then following rule is followed
!
! For scalar, ans = 1
! For vector, ans = obj%s(1)
! For matrix, and = obj%s(1) * obj%s(2)

INTERFACE Size
  MODULE PURE FUNCTION fevar_Size(obj, Dim) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dim
    INTEGER(I4B) :: ans
  END FUNCTION fevar_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                           SHAPE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the shape of data
!
!# Introduction
!
! ans depends on the rank and vartype
!
!| rank | vartype | ans |
!| --- | --- | ---  |
!| Scalar | Constant | [1] |
!| Scalar | Space, Time | [obj%s(1)] |
!| Scalar | SpaceTime | [obj%s(1), obj%s(2)] |
!| Vector | Constant | [obj%s(1)] |
!| Vector | Space, Time | [obj%s(1), obj%s(2)] |
!| Vector | SpaceTime | [obj%s(1), obj%s(2), obj%s(3)] |
!| Matrix | Constant | [obj%s(1), obj%s(2)] |
!| Matrix | Space, Time | [obj%s(1), obj%s(2), obj%s(3)] |
!| Matrix | SpaceTime | [obj%s(1), obj%s(2), obj%s(3), obj%s(4)] |

INTERFACE Shape
  MODULE PURE FUNCTION fevar_Shape(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION fevar_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                            rank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the rank of FEvariable

INTERFACE OPERATOR(.RANK.)
  MODULE PURE FUNCTION fevar_rank(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_rank
END INTERFACE

!----------------------------------------------------------------------------
!                                                        vartype@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the vartype of FEvariable

INTERFACE OPERATOR(.vartype.)
  MODULE PURE FUNCTION fevar_vartype(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_vartype
END INTERFACE

!----------------------------------------------------------------------------
!                                                       defineon@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE OPERATOR(.defineon.)
  MODULE PURE FUNCTION fevar_defineon(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_defineon
END INTERFACE

!----------------------------------------------------------------------------
!                                                isNodalVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE isNodalVariable
  MODULE PURE FUNCTION fevar_isNodalVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isNodalVariable
END INTERFACE isNodalVariable

!----------------------------------------------------------------------------
!                                            isQuadratureVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE isQuadratureVariable
  MODULE PURE FUNCTION fevar_isQuadratureVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isQuadratureVariable
END INTERFACE isQuadratureVariable

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, constant

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP) :: val
  END FUNCTION Scalar_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, space

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Scalar_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is scalar, space without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Space_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE Scalar_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, time

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Scalar_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is scalar, time without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Time_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE Scalar_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, SpaceTime

INTERFACE Get
  MODULE PURE FUNCTION Scalar_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Scalar_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is scalar, SpaceTime without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_SpaceTime_(obj, rank, vartype, val, &
                                           nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Scalar_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, constant

INTERFACE Get
  MODULE PURE FUNCTION Vector_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Vector_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is vector, constant without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Constant_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE Vector_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, space

INTERFACE Get
  MODULE PURE FUNCTION Vector_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Vector_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is vector, space without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Space_(obj, rank, vartype, val, &
                                       nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Vector_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, time

INTERFACE Get
  MODULE PURE FUNCTION Vector_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Vector_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is vector, time without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Time_(obj, rank, vartype, val, &
                                      nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Vector_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, spaceTime

INTERFACE Get
  MODULE PURE FUNCTION Vector_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Vector_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is vector, spaceTime without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_SpaceTime_(obj, rank, vartype, val, &
                                           dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE Vector_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Constant

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Matrix_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is Matrix, Constant without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Constant_(obj, rank, vartype, val, &
                                          nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(inout) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Matrix_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Space

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Matrix_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is Matrix, Space without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Space_(obj, rank, vartype, val, &
                                       dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE Matrix_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Time

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Matrix_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is Matrix, Time without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Time_(obj, rank, vartype, val, &
                                      dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE Matrix_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, SpaceTime

INTERFACE Get
  MODULE PURE FUNCTION Matrix_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :, :)
  END FUNCTION Matrix_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  Returns value which is Matrix, SpaceTime without allocation

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_SpaceTime_(obj, rank, vartype, val, &
                                           dim1, dim2, dim3, dim4)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(INOUT) :: val(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE Matrix_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION fevar_Addition1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + Real

INTERFACE OPERATOR(+)

  MODULE PURE FUNCTION fevar_Addition2(obj1, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real + FEVariable

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION fevar_Addition3(val, obj1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition3
END INTERFACE

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - FEVariable

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION fevar_Subtraction1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction1
END INTERFACE

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - RealVal

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION fevar_Subtraction2(obj1, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction2
END INTERFACE

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = RealVal - FEVariable

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION fevar_Subtraction3(val, obj1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction3
END INTERFACE

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-1
! summary: FEVariable = FEVariable * FEVariable

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION fevar_Multiplication1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication1
END INTERFACE

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable * Real

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION fevar_Multiplication2(obj1, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication2
END INTERFACE

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real * FEVariable

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION fevar_Multiplication3(val, obj1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication3
END INTERFACE

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE ABS
  MODULE PURE FUNCTION fevar_abs(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_abs
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION fevar_dot_product(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_dot_product
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - FEVariable

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION fevar_Division1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - Real

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION fevar_Division2(obj1, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real - FEVariable

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION fevar_Division3(val, obj1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE OPERATOR(**)
  MODULE PURE FUNCTION fevar_power(obj, n) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_power
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE SQRT
  MODULE PURE FUNCTION fevar_sqrt(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_sqrt
END INTERFACE

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE NORM2
  MODULE PURE FUNCTION fevar_norm2(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_norm2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE OPERATOR(.EQ.)
  MODULE PURE FUNCTION fevar_isEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isEqual
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE OPERATOR(.NE.)
  MODULE PURE FUNCTION fevar_notEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_notEqual
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean1(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Mean1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean2(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: dataType
    REAL(DFP) :: ans
  END FUNCTION fevar_Mean2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean3(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION fevar_Mean3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean4(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION fevar_Mean4
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_(obj, rank, vartype, &
                                                         N, nns, nips, &
                                                         scale, &
                                                         addContribution, &
                                                         ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
  END SUBROUTINE ScalarConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_(obj, rank, vartype, &
                                                      N, nns, nips, &
                                                      scale, &
                                                      addContribution, &
                                                      ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
  END SUBROUTINE ScalarSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_(obj, rank, &
                                                          vartype, &
                                                          N, nns, nips, &
                                                          T, nnt, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, tsize, &
                                                          timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorConstantGetInterpolation_(obj, rank, vartype, &
                                                         N, nns, nips, &
                                                         scale, &
                                                         addContribution, &
                                                         ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
  END SUBROUTINE VectorConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_(obj, rank, vartype, &
                                                      N, nns, nips, &
                                                      scale, &
                                                      addContribution, &
                                                      ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
  END SUBROUTINE VectorSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_(obj, rank, &
                                                          vartype, &
                                                          N, nns, nips, &
                                                          T, nnt, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, nrow, ncol, &
                                                          timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE VectorSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixConstantGetInterpolation_(obj, rank, vartype, &
                                                         N, nns, nips, &
                                                         scale, &
                                                         addContribution, &
                                                         ans, dim1, dim2, &
                                                         dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
  END SUBROUTINE MatrixConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@MatrixInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceGetInterpolation_(obj, rank, vartype, &
                                                      N, nns, nips, &
                                                      scale, &
                                                      addContribution, &
                                                      ans, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
  END SUBROUTINE MatrixSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@MatrixInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceTimeGetInterpolation_(obj, rank, &
                                                          vartype, &
                                                          N, nns, nips, &
                                                          T, nnt, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, dim1, dim2, &
                                                          dim3, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE FEVariableGetInterpolation_1(obj, N, nns, nips, &
                                                     scale, addContribution, &
                                                     ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value in FEVariable_ format
  END SUBROUTINE FEVariableGetInterpolation_1
END INTERFACE GetInterpolation_

END MODULE FEVariable_Method
