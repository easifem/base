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

!> author: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	This subroutine contains algebraic operator

SUBMODULE(Rank2Tensor_Method) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add_obj
  Ans%T = obj1%T + obj2%T
  IF( obj1%isSym .AND. obj2%isSym ) Ans%isSym = .TRUE.
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add_Scalar
  Ans%T = obj1%T + obj2
  Ans%isSym = obj1%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_add_obj
  Ans%T = obj1 + obj2%T
  Ans%isSym = obj2%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_minus_obj
  Ans%T = obj1%T - obj2%T
  IF( obj1%isSym .AND. obj2%isSym ) Ans%isSym = .TRUE.

END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_minus_Scalar
  Ans%T = obj1%T - obj2
  Ans%isSym = obj1%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_minus_obj
  Ans%T = obj1 - obj2%T
  Ans%isSym = obj2%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_times_obj
  Ans%T = obj1%T * obj2%T
END PROCEDURE obj_times_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_times_scalar
  Ans%T = obj1%T * obj2
  Ans%isSym = obj1%isSym
END PROCEDURE obj_times_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_times_obj
  Ans%T = obj1 * obj2%T
  Ans%isSym = obj2%isSym
END PROCEDURE scalar_times_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_div_obj
  Ans%T = obj1%T / obj2%T
END PROCEDURE obj_div_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_div_scalar
  Ans%T = obj1%T / obj2
  Ans%isSym = obj1%isSym
END PROCEDURE obj_div_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_div_obj
  Ans%T = obj1 / obj2%T
  Ans%isSym = obj2%isSym
END PROCEDURE scalar_div_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_matmul_obj
  Ans%T = MATMUL( obj1%T, obj2%T )
END PROCEDURE obj_matmul_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_matmul_vec
  Ans = MATMUL( obj1%T, obj2 )
END PROCEDURE obj_matmul_vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_matmul_obj
  Ans = MATMUL( obj1, obj2%T )
END PROCEDURE vec_matmul_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE OperatorMethods