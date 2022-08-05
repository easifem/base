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

PUBLIC :: QuadratureVariable

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