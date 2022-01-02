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

PUBLIC :: SIZE
PUBLIC :: SHAPE
PUBLIC :: OPERATOR(.RANK.)
PUBLIC :: OPERATOR(.vartype.)
PUBLIC :: OPERATOR(.defineon.)
PUBLIC :: isNodalVariable
PUBLIC :: isQuadratureVariable
PUBLIC :: Get

!----------------------------------------------------------------------------
!                                                            SIZE@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION fevar_Size(obj, Dim) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Dim
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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