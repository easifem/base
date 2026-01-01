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

SUBMODULE(FEVariable_AdditionMethod) Methods
USE FEVariable_GetMethod, ONLY: GetRankCase
USE FEVariable_GetMethod, ONLY: GetVarCase
USE FEVariable_Scalar_Scalar_Addition, ONLY: Scalar_Scalar_Master
USE FEVariable_Scalar_Vector_Addition, ONLY: Scalar_Vector_Master
USE FEVariable_Scalar_Matrix_Addition, ONLY: Scalar_Matrix_Master
USE FEVariable_Vector_Scalar_Addition, ONLY: Vector_Scalar_Master
USE FEVariable_Vector_Vector_Addition, ONLY: Vector_Vector_Master
USE FEVariable_Matrix_Scalar_Addition, ONLY: Matrix_Scalar_Master
USE FEVariable_Matrix_Matrix_Addition, ONLY: Matrix_Matrix_Master
USE BaseType, ONLY: varopt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition_1
INTEGER(I4B) :: rankCase, varCase
rankCase = GetRankCase(obj1%rank, obj2%rank)
varCase = GetRankCase(obj1%varType, obj2%varType)
CALL Addition_(obj1, obj2, ans, rankCase, varCase)
END PROCEDURE fevar_Addition_1

!----------------------------------------------------------------------------
!                                                             Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition_2
ans%len = obj%len
ans%s = obj%s
ans%val(1:ans%len) = obj%val(1:ans%len) + val
END PROCEDURE fevar_Addition_2

!----------------------------------------------------------------------------
!                                                                 Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition_3
SELECT CASE (rankCase)

CASE (00)
  CALL Scalar_Scalar_Master(obj1, obj2, ans, varCase)

CASE (01)
  CALL Scalar_Vector_Master(obj1, obj2, ans, varCase)

CASE (02)
  CALL Scalar_Matrix_master(obj1, obj2, ans, varCase)

CASE (10)
  CALL Vector_Scalar_master(obj1, obj2, ans, varCase)

CASE (11)
  CALL Vector_Vector_master(obj1, obj2, ans, varCase)

CASE (20)
  CALL Matrix_Scalar_master(obj1, obj2, ans, varCase)

CASE (22)
  CALL Matrix_Matrix_master(obj1, obj2, ans, varCase)

END SELECT
END PROCEDURE fevar_Addition_3

!----------------------------------------------------------------------------
!                                                             Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition1
END PROCEDURE fevar_Addition1

!----------------------------------------------------------------------------
!                                                             Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition2
END PROCEDURE fevar_Addition2

!----------------------------------------------------------------------------
!                                                             Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Addition3
END PROCEDURE fevar_Addition3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

