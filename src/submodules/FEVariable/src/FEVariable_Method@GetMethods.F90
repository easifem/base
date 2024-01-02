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

SUBMODULE(FEVariable_Method) GetMethods
USE BaseMethod, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                GetLambdaFromYoungsModulus
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_GetLambdaFromYoungsModulus
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

isok = ALLOCATED(youngsModulus%val)

IF (isok) THEN
  tsize = SIZE(youngsModulus%val)
ELSE
  tsize = 0
END IF

CALL Reallocate(lambda%val, tsize)

DO ii = 1, tsize
  lambda%val(1:tsize) = shearModulus%val *  &
    & (youngsModulus%val - 2.0_DFP * shearModulus%val) /  &
    & (3.0_DFP * shearModulus%val - youngsModulus%val)
END DO

lambda%s = youngsModulus%s
lambda%defineOn = youngsModulus%defineOn
lambda%varType = youngsModulus%varType
lambda%rank = youngsModulus%rank
END PROCEDURE fevar_GetLambdaFromYoungsModulus

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Size
IF (PRESENT(dim)) THEN
  ans = obj%s(dim)
ELSE
  SELECT CASE (obj%rank)
  CASE (Scalar)
    ans = 1
  CASE (Vector)
    ans = obj%s(1)
  CASE (Matrix)
    ans = obj%s(1) * obj%s(2)
  END SELECT
END IF
END PROCEDURE fevar_Size

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Shape
SELECT CASE (obj%rank)
CASE (Scalar)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = [1]
  CASE (Space, Time)
    ans = obj%s(1:1)
  CASE (SpaceTime)
    ans = obj%s(1:2)
  END SELECT
CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:1)
  CASE (Space, Time)
    ans = obj%s(1:2)
  CASE (SpaceTime)
    ans = obj%s(1:3)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:2)
  CASE (Space, Time)
    ans = obj%s(1:3)
  CASE (SpaceTime)
    ans = obj%s(1:4)
  END SELECT
END SELECT
END PROCEDURE fevar_Shape

!----------------------------------------------------------------------------
!                                                                      rank
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_rank
ans = obj%rank
END PROCEDURE fevar_rank

!----------------------------------------------------------------------------
!                                                                    vartype
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_vartype
ans = obj%vartype
END PROCEDURE fevar_vartype

!----------------------------------------------------------------------------
!                                                                   defineon
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_defineon
ans = obj%defineon
END PROCEDURE fevar_defineon

!----------------------------------------------------------------------------
!                                                            isNodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isNodalVariable
IF (obj%defineon .EQ. nodal) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE fevar_isNodalVariable

!----------------------------------------------------------------------------
!                                                            isNodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isQuadratureVariable
IF (obj%defineon .EQ. nodal) THEN
  ans = .FALSE.
ELSE
  ans = .TRUE.
END IF
END PROCEDURE fevar_isQuadratureVariable

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Constant
val = obj%val(1)
END PROCEDURE Scalar_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space
val = obj%val
END PROCEDURE Scalar_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Time
val = obj%val
END PROCEDURE Scalar_Time

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime
val = RESHAPE(obj%val, obj%s(1:2))
END PROCEDURE Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant
val = obj%val
END PROCEDURE Vector_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space
val = RESHAPE(obj%val, obj%s(1:2))
END PROCEDURE Vector_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Time
val = RESHAPE(obj%val, obj%s(1:2))
END PROCEDURE Vector_Time

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime
val = RESHAPE(obj%val, obj%s(1:3))
END PROCEDURE Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant
val = RESHAPE(obj%val, obj%s(1:2))
END PROCEDURE Matrix_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space
val = RESHAPE(obj%val, obj%s(1:3))
END PROCEDURE Matrix_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Time
val = RESHAPE(obj%val, obj%s(1:3))
END PROCEDURE Matrix_Time

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime
val = RESHAPE(obj%val, obj%s(1:4))
END PROCEDURE Matrix_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
