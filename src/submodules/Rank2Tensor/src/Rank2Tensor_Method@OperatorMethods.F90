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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Algebraic operators for [[Rank2Tensor_]]
!
! Description:
!   `*` and `/` between two tensors are element-wise. `MATMUL` is the
!   single contraction.

SUBMODULE(Rank2Tensor_Method) OperatorMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                          +
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AddObjObj
ans%T = obj1%T + obj2%T
ans%isSym = obj1%isSym .AND. obj2%isSym
END PROCEDURE Rank2Tensor_AddObjObj

!----------------------------------------------------------------------------
!                                                                          +
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AddObjScalar
ans%T = obj1%T + obj2
ans%isSym = obj1%isSym
END PROCEDURE Rank2Tensor_AddObjScalar

!----------------------------------------------------------------------------
!                                                                          +
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_AddScalarObj
ans%T = obj1 + obj2%T
ans%isSym = obj2%isSym
END PROCEDURE Rank2Tensor_AddScalarObj

!----------------------------------------------------------------------------
!                                                                          -
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MinusObjObj
ans%T = obj1%T - obj2%T
ans%isSym = obj1%isSym .AND. obj2%isSym
END PROCEDURE Rank2Tensor_MinusObjObj

!----------------------------------------------------------------------------
!                                                                          -
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MinusObjScalar
ans%T = obj1%T - obj2
ans%isSym = obj1%isSym
END PROCEDURE Rank2Tensor_MinusObjScalar

!----------------------------------------------------------------------------
!                                                                          -
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MinusScalarObj
ans%T = obj1 - obj2%T
ans%isSym = obj2%isSym
END PROCEDURE Rank2Tensor_MinusScalarObj

!----------------------------------------------------------------------------
!                                                                          *
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_TimesObjObj
ans%T = obj1%T * obj2%T
ans%isSym = obj1%isSym .AND. obj2%isSym
END PROCEDURE Rank2Tensor_TimesObjObj

!----------------------------------------------------------------------------
!                                                                          *
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_TimesObjScalar
ans%T = obj1%T * obj2
ans%isSym = obj1%isSym
END PROCEDURE Rank2Tensor_TimesObjScalar

!----------------------------------------------------------------------------
!                                                                          *
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_TimesScalarObj
ans%T = obj1 * obj2%T
ans%isSym = obj2%isSym
END PROCEDURE Rank2Tensor_TimesScalarObj

!----------------------------------------------------------------------------
!                                                                          /
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DivObjObj
ans%T = obj1%T / obj2%T
ans%isSym = obj1%isSym .AND. obj2%isSym
END PROCEDURE Rank2Tensor_DivObjObj

!----------------------------------------------------------------------------
!                                                                          /
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DivObjScalar
ans%T = obj1%T / obj2
ans%isSym = obj1%isSym
END PROCEDURE Rank2Tensor_DivObjScalar

!----------------------------------------------------------------------------
!                                                                          /
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DivScalarObj
ans%T = obj1 / obj2%T
ans%isSym = obj2%isSym
END PROCEDURE Rank2Tensor_DivScalarObj

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MatmulObjObj
ans%T = MATMUL(obj1%T, obj2%T)
ans%isSym = .FALSE.
END PROCEDURE Rank2Tensor_MatmulObjObj

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MatmulObjVec
ans = MATMUL(obj1%T, obj2)
END PROCEDURE Rank2Tensor_MatmulObjVec

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MatmulVecObj
ans = MATMUL(obj1, obj2%T)
END PROCEDURE Rank2Tensor_MatmulVecObj

END SUBMODULE OperatorMethods
