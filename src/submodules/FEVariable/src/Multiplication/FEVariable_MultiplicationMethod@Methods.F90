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

#define _OP_ *

SUBMODULE(FEVariable_MultiplicationMethod) Methods
USE FEVariable_GetMethod, ONLY: GetRankCase
USE FEVariable_Scalar_Scalar_Multiplication, ONLY: Scalar_Scalar_Master
USE FEVariable_Scalar_Vector_Multiplication, ONLY: Scalar_Vector_Master
USE FEVariable_Scalar_Matrix_Multiplication, ONLY: Scalar_Matrix_Master
USE FEVariable_Vector_Scalar_Multiplication, ONLY: Vector_Scalar_Master
USE FEVariable_Vector_Vector_Multiplication, ONLY: Vector_Vector_Master
USE FEVariable_Matrix_Scalar_Multiplication, ONLY: Matrix_Scalar_Master
USE FEVariable_Matrix_Matrix_Multiplication, ONLY: Matrix_Matrix_Master
USE BaseType, ONLY: varopt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication1
END PROCEDURE fevar_Multiplication1

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication_1
INTEGER(I4B) :: rankCase

rankCase = GetRankCase(obj1%rank, obj2%rank)

SELECT CASE (rankCase)

CASE (00)
  CALL Scalar_Scalar_Master(obj1, obj2, ans)

CASE (01)
  CALL Scalar_Vector_Master(obj1, obj2, ans)

CASE (02)
  CALL Scalar_Matrix_master(obj1, obj2, ans)

CASE (10)
  CALL Vector_Scalar_master(obj1, obj2, ans)

CASE (11)
  CALL Vector_Vector_master(obj1, obj2, ans)

CASE (20)
  CALL Matrix_Scalar_master(obj1, obj2, ans)

CASE (22)
  CALL Matrix_Matrix_master(obj1, obj2, ans)

END SELECT

END PROCEDURE fevar_Multiplication_1

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication2
END PROCEDURE fevar_Multiplication2

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication_2
ans%len = obj%len
ans%s = obj%s
ans%val(1:ans%len) = obj%val(1:ans%len) _OP_ val
END PROCEDURE fevar_Multiplication_2

!----------------------------------------------------------------------------
!                                                             Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Multiplication3
END PROCEDURE fevar_Multiplication3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

#undef _OP_
