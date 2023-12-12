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
USE BaseType
USE GlobalData
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

!----------------------------------------------------------------------------
!                                 GetLambdaFromYoungsModulus@SpecialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-12
! summary:  Get lame parameter lambda from YoungsModulus

INTERFACE GetLambdaFromYoungsModulus
  MODULE PURE SUBROUTINE fevar_GetLambdaFromYoungsModulus(youngsModulus,  &
    & shearModulus, lambda)
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

INTERFACE
  MODULE SUBROUTINE fevar_Display(obj, Msg, UnitNo)
    TYPE(FEVariable_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE fevar_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE fevar_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Constant

INTERFACE
  MODULE PURE FUNCTION Quadrature_Scalar_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Space

INTERFACE
  MODULE PURE FUNCTION Quadrature_Scalar_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, Time

INTERFACE
  MODULE PURE FUNCTION Quadrature_Scalar_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_Time
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Scalar, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Scalar_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Constant

INTERFACE
  MODULE PURE FUNCTION Quadrature_Vector_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Space

INTERFACE
  MODULE PURE FUNCTION Quadrature_Vector_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, Time

INTERFACE
  MODULE PURE FUNCTION Quadrature_Vector_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_Time
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Vector, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Quadrature_Vector_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Vector_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Constant

INTERFACE
  MODULE PURE FUNCTION Quadrature_Matrix_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Space

INTERFACE
  MODULE PURE FUNCTION Quadrature_Matrix_Space(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, Time

INTERFACE
  MODULE PURE FUNCTION Quadrature_Matrix_Time(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_Time
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_Time
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                      QuadratureVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create quadrature variable, which is Matrix, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Quadrature_Matrix_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Deallocates the content of FEVariable

INTERFACE
  MODULE PURE SUBROUTINE fevar_Deallocate(obj)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
  END SUBROUTINE fevar_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE fevar_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, constant

INTERFACE
  MODULE PURE FUNCTION Nodal_Scalar_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariableScalar_), INTENT(IN) :: rank
    CLASS(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, Space

INTERFACE
  MODULE PURE FUNCTION Nodal_Scalar_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, Time

INTERFACE
  MODULE PURE FUNCTION Nodal_Scalar_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_Time
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is scalar, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Nodal_Scalar_SpaceTime(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Scalar_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Constant

INTERFACE
  MODULE PURE FUNCTION Nodal_Vector_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Space

INTERFACE
  MODULE PURE FUNCTION Nodal_Vector_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, Time

INTERFACE
  MODULE PURE FUNCTION Nodal_Vector_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_Time
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is vector, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Nodal_Vector_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Vector_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Constant

INTERFACE
  MODULE PURE FUNCTION Nodal_Matrix_Constant(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Space

INTERFACE
  MODULE PURE FUNCTION Nodal_Matrix_Space(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, Time

INTERFACE
  MODULE PURE FUNCTION Nodal_Matrix_Time(val, rank, vartype) RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_Time
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_Time
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                           NodalVariable@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Create nodal variable, which is Matrix, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Nodal_Matrix_SpaceTime(val, rank, vartype) &
    & RESULT(obj)
    TYPE(FEVariable_) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
  END FUNCTION Nodal_Matrix_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                            SIZE@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION fevar_Size(obj, Dim) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dim
    INTEGER(I4B) :: Ans
  END FUNCTION fevar_Size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE fevar_Size
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                           SHAPE@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION fevar_Shape(obj) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION fevar_Shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE fevar_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                            rank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the rank of FEvariable

INTERFACE
  MODULE PURE FUNCTION fevar_rank(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_rank
END INTERFACE

INTERFACE OPERATOR(.RANK.)
  MODULE PROCEDURE fevar_rank
END INTERFACE OPERATOR(.RANK.)

!----------------------------------------------------------------------------
!                                                        vartype@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the vartype of FEvariable

INTERFACE
  MODULE PURE FUNCTION fevar_vartype(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_vartype
END INTERFACE

INTERFACE OPERATOR(.vartype.)
  MODULE PROCEDURE fevar_vartype
END INTERFACE OPERATOR(.varType.)

!----------------------------------------------------------------------------
!                                                       defineon@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE
  MODULE PURE FUNCTION fevar_defineon(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION fevar_defineon
END INTERFACE

INTERFACE OPERATOR(.defineon.)
  MODULE PROCEDURE fevar_defineon
END INTERFACE OPERATOR(.defineon.)

!----------------------------------------------------------------------------
!                                                isNodalVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE
  MODULE PURE FUNCTION fevar_isNodalVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isNodalVariable
END INTERFACE

INTERFACE isNodalVariable
  MODULE PROCEDURE fevar_isNodalVariable
END INTERFACE isNodalVariable

!----------------------------------------------------------------------------
!                                            isQuadratureVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE
  MODULE PURE FUNCTION fevar_isQuadratureVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isQuadratureVariable
END INTERFACE

INTERFACE isQuadratureVariable
  MODULE PROCEDURE fevar_isQuadratureVariable
END INTERFACE isQuadratureVariable

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, constant

INTERFACE
  MODULE PURE FUNCTION Scalar_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP) :: val
  END FUNCTION Scalar_Constant
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Scalar_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, space

INTERFACE
  MODULE PURE FUNCTION Scalar_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Scalar_Space
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Scalar_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, time

INTERFACE
  MODULE PURE FUNCTION Scalar_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Scalar_Time
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Scalar_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is scalar, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Scalar_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Scalar_SpaceTime
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Scalar_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, constant

INTERFACE
  MODULE PURE FUNCTION Vector_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION Vector_Constant
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Vector_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, space

INTERFACE
  MODULE PURE FUNCTION Vector_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Vector_Space
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Vector_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, time

INTERFACE
  MODULE PURE FUNCTION Vector_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Vector_Time
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Vector_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is vector, spaceTime

INTERFACE
  MODULE PURE FUNCTION Vector_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Vector_SpaceTime
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Vector_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Constant

INTERFACE
  MODULE PURE FUNCTION Matrix_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :)
  END FUNCTION Matrix_Constant
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Matrix_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Space

INTERFACE
  MODULE PURE FUNCTION Matrix_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Matrix_Space
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Matrix_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, Time

INTERFACE
  MODULE PURE FUNCTION Matrix_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
  END FUNCTION Matrix_Time
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Matrix_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Jan 2022
! summary: Returns value which is Matrix, SpaceTime

INTERFACE
  MODULE PURE FUNCTION Matrix_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), ALLOCATABLE :: val(:, :, :, :)
  END FUNCTION Matrix_SpaceTime
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE Matrix_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Addition1(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition1
END INTERFACE

INTERFACE OPERATOR(+)
  MODULE PROCEDURE fevar_Addition1
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + Real

INTERFACE
  MODULE PURE FUNCTION fevar_Addition2(obj1, val) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition2
END INTERFACE

INTERFACE OPERATOR(+)
  MODULE PROCEDURE fevar_Addition2
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                   Addition@AdditioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Addition3(val, obj1) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Addition3
END INTERFACE

INTERFACE OPERATOR(+)
  MODULE PROCEDURE fevar_Addition3
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction1(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction1
END INTERFACE

INTERFACE OPERATOR(-)
  MODULE PROCEDURE fevar_Subtraction1
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - RealVal

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction2(obj1, val) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction2
END INTERFACE

INTERFACE OPERATOR(-)
  MODULE PROCEDURE fevar_Subtraction2
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                            Substraction@SubstractioMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = RealVal - FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction3(val, obj1) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction3
END INTERFACE

INTERFACE OPERATOR(-)
  MODULE PROCEDURE fevar_Subtraction3
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-1
! summary: FEVariable = FEVariable * FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Multiplication1(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication1
END INTERFACE

INTERFACE OPERATOR(*)
  MODULE PROCEDURE fevar_Multiplication1
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable * Real

INTERFACE
  MODULE PURE FUNCTION fevar_Multiplication2(obj1, val) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication2
END INTERFACE

INTERFACE OPERATOR(*)
  MODULE PROCEDURE fevar_Multiplication2
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                      Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real * FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Multiplication3(val, obj1) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Multiplication3
END INTERFACE

INTERFACE OPERATOR(*)
  MODULE PROCEDURE fevar_Multiplication3
END INTERFACE OPERATOR(*)

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_abs(obj) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_abs
END INTERFACE

INTERFACE ABS
  MODULE PROCEDURE fevar_abs
END INTERFACE ABS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_dot_product(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_dot_product
END INTERFACE

INTERFACE DOT_PRODUCT
  MODULE PROCEDURE fevar_dot_product
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Division1(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division1
END INTERFACE

INTERFACE OPERATOR(/)
  MODULE PROCEDURE fevar_Division1
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable - Real

INTERFACE
  MODULE PURE FUNCTION fevar_Division2(obj1, val) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division2
END INTERFACE

INTERFACE OPERATOR(/)
  MODULE PROCEDURE fevar_Division2
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                   Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = Real - FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Division3(val, obj1) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Division3
END INTERFACE

INTERFACE OPERATOR(/)
  MODULE PROCEDURE fevar_Division3
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                   Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_power(obj, n) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_power
END INTERFACE

INTERFACE OPERATOR(**)
  MODULE PROCEDURE fevar_power
END INTERFACE OPERATOR(**)

!----------------------------------------------------------------------------
!                                                   Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_sqrt(obj) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_sqrt
END INTERFACE

INTERFACE SQRT
  MODULE PROCEDURE fevar_sqrt
END INTERFACE SQRT

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE
  MODULE PURE FUNCTION fevar_norm2(obj) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_norm2
END INTERFACE

INTERFACE NORM2
  MODULE PROCEDURE fevar_norm2
END INTERFACE NORM2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE
  MODULE PURE FUNCTION fevar_isEqual(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isEqual
END INTERFACE

INTERFACE OPERATOR(.EQ.)
  MODULE PROCEDURE fevar_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE
  MODULE PURE FUNCTION fevar_notEqual(obj1, obj2) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_notEqual
END INTERFACE

INTERFACE OPERATOR(.NE.)
  MODULE PROCEDURE fevar_notEqual
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE
  MODULE PURE FUNCTION fevar_Mean1(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Mean1
END INTERFACE

INTERFACE MEAN
  MODULE PROCEDURE fevar_Mean1
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE
  MODULE PURE FUNCTION fevar_Mean2(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: dataType
    REAL(DFP) :: ans
  END FUNCTION fevar_Mean2
END INTERFACE

INTERFACE MEAN
  MODULE PROCEDURE fevar_Mean2
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE
  MODULE PURE FUNCTION fevar_Mean3(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION fevar_Mean3
END INTERFACE

INTERFACE MEAN
  MODULE PROCEDURE fevar_Mean3
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 May 2022
! summary: FEVariable = Mean( obj )

INTERFACE
  MODULE PURE FUNCTION fevar_Mean4(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION fevar_Mean4
END INTERFACE

INTERFACE MEAN
  MODULE PROCEDURE fevar_Mean4
END INTERFACE MEAN

END MODULE FEVariable_Method
