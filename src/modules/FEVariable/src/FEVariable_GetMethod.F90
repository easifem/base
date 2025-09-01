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

MODULE FEVariable_GetMethod
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

PUBLIC :: SIZE
PUBLIC :: SHAPE
PUBLIC :: OPERATOR(.RANK.)
PUBLIC :: OPERATOR(.vartype.)
PUBLIC :: OPERATOR(.defineon.)
PUBLIC :: isNodalVariable
PUBLIC :: isQuadratureVariable
PUBLIC :: FEVariable_ToChar
PUBLIC :: FEVariable_ToInteger
PUBLIC :: GetLambdaFromYoungsModulus

PUBLIC :: Get
PUBLIC :: Get_

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
!                                                IsNodalVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE IsNodalVariable
  MODULE PURE FUNCTION fevar_IsNodalVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_IsNodalVariable
END INTERFACE IsNodalVariable

!----------------------------------------------------------------------------
!                                            isQuadratureVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the defineon of FEvariable

INTERFACE IsQuadratureVariable
  MODULE PURE FUNCTION fevar_IsQuadratureVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_IsQuadratureVariable
END INTERFACE IsQuadratureVariable

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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!                                                              Get@GetMethods
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
!                                                             Get_@GetMethods
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
!
!----------------------------------------------------------------------------

END MODULE FEVariable_GetMethod
