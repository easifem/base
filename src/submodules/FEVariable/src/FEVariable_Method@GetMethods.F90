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

SUBMODULE(FEVariable_Method) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE Size_obj
SELECT CASE (obj%Rank)
CASE (Scalar)
  Ans = 1
CASE (Vector)
  SELECT CASE (obj%VarType)
  CASE (Constant)
    Ans = SIZE(obj%R1)
  CASE (Space)
    Ans = SIZE(obj%R2, 1)
  CASE (SpaceTime)
    Ans = SIZE(obj%R3, 1)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%VarType)
  CASE (Constant)
    Ans = SIZE(obj%R2, Dim)
  CASE (Space)
    Ans = SIZE(obj%R3, Dim)
  CASE (SpaceTime)
    Ans = SIZE(obj%R4, Dim)
  END SELECT
END SELECT
END PROCEDURE Size_obj

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE Shape_obj
SELECT CASE (obj%Rank)
CASE (Scalar)
  SELECT CASE (obj%VarType)
  CASE (Constant)
    Ans = [1]
  CASE (Space)
    Ans = SHAPE(obj%R1)
  CASE (SpaceTime)
    Ans = SHAPE(obj%R2)
  END SELECT
CASE (Vector)
  SELECT CASE (obj%VarType)
  CASE (Constant)
    Ans = SHAPE(obj%R1)
  CASE (Space)
    Ans = SHAPE(obj%R2)
  CASE (SpaceTime)
    Ans = SHAPE(obj%R3)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%VarType)
  CASE (Constant)
    Ans = SHAPE(obj%R2)
  CASE (Space)
    Ans = SHAPE(obj%R3)
  CASE (SpaceTime)
    Ans = SHAPE(obj%R4)
  END SELECT
END SELECT
END PROCEDURE Shape_obj

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Constant
Val = obj%R0
END PROCEDURE Scalar_Constant

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space
Val = obj%R1
END PROCEDURE Scalar_Space

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime
Val = obj%R2
END PROCEDURE Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant
Val = obj%R1
END PROCEDURE Vector_Constant

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space
Val = obj%R2
END PROCEDURE Vector_Space

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime
Val = obj%R3
END PROCEDURE Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant
Val = obj%R2
END PROCEDURE Matrix_Constant

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space
Val = obj%R3
END PROCEDURE Matrix_Space

!----------------------------------------------------------------------------
!                                                            getNodalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime
Val = obj%R4
END PROCEDURE Matrix_SpaceTime

END SUBMODULE GetMethods
