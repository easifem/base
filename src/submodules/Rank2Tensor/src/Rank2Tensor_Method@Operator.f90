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

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	This subroutine contains algebraic operator

SUBMODULE( Rank2Tensor_Method ) Operator
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Obj_add_Obj
  Ans%T = Obj1%T + Obj2%T
  IF( Obj1%isSym .AND. Obj2%isSym ) Ans%isSym = .TRUE.
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Obj_add_Scalar
  Ans%T = Obj1%T + Obj2
  Ans%isSym = Obj1%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_add_Obj
  Ans%T = Obj1 + Obj2%T
  Ans%isSym = Obj2%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Obj_minus_Obj
  Ans%T = Obj1%T - Obj2%T
  IF( Obj1%isSym .AND. Obj2%isSym ) Ans%isSym = .TRUE.

END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Obj_minus_Scalar
  Ans%T = Obj1%T - Obj2
  Ans%isSym = Obj1%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_minus_Obj
  Ans%T = Obj1 - Obj2%T
  Ans%isSym = Obj2%isSym
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_times_obj
  Ans%T = Obj1%T * Obj2%T
END PROCEDURE obj_times_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_times_scalar
  Ans%T = Obj1%T * Obj2
  Ans%isSym = Obj1%isSym
END PROCEDURE obj_times_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_times_obj
  Ans%T = Obj1 * Obj2%T
  Ans%isSym = Obj2%isSym
END PROCEDURE scalar_times_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_div_obj
  Ans%T = Obj1%T / Obj2%T
END PROCEDURE obj_div_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_div_scalar
  Ans%T = Obj1%T / Obj2
  Ans%isSym = Obj1%isSym
END PROCEDURE obj_div_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalar_div_obj
  Ans%T = Obj1 / Obj2%T
  Ans%isSym = Obj2%isSym
END PROCEDURE scalar_div_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_matmul_obj
  Ans%T = MATMUL( Obj1%T, Obj2%T )
END PROCEDURE obj_matmul_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_matmul_vec
  Ans = MATMUL( Obj1%T, Obj2 )
END PROCEDURE obj_matmul_vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_matmul_obj
  Ans = MATMUL( Obj1, Obj2%T )
END PROCEDURE vec_matmul_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Operator