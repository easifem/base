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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Size
SELECT CASE (obj%rank)
CASE (Scalar)
  ans = 1
CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = SIZE(obj%r1)
  CASE (Space)
    ans = SIZE(obj%r2, 1)
  CASE (SpaceTime)
    ans = SIZE(obj%r3, 1)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = SIZE(obj%r2, Dim)
  CASE (Space)
    ans = SIZE(obj%r3, Dim)
  CASE (SpaceTime)
    ans = SIZE(obj%r4, Dim)
  END SELECT
END SELECT
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
  CASE (Space)
    ans = SHAPE(obj%r1)
  CASE (SpaceTime)
    ans = SHAPE(obj%r2)
  END SELECT
CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = SHAPE(obj%r1)
  CASE (Space)
    ans = SHAPE(obj%r2)
  CASE (SpaceTime)
    ans = SHAPE(obj%r3)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = SHAPE(obj%r2)
  CASE (Space)
    ans = SHAPE(obj%r3)
  CASE (SpaceTime)
    ans = SHAPE(obj%r4)
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
val = obj%r0
END PROCEDURE Scalar_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space
val = obj%r1
END PROCEDURE Scalar_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime
val = obj%r2
END PROCEDURE Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant
val = obj%r1
END PROCEDURE Vector_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space
val = obj%r2
END PROCEDURE Vector_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime
val = obj%r3
END PROCEDURE Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant
val = obj%r2
END PROCEDURE Matrix_Constant

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space
val = obj%r3
END PROCEDURE Matrix_Space

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime
val = obj%r4
END PROCEDURE Matrix_SpaceTime

END SUBMODULE GetMethods
