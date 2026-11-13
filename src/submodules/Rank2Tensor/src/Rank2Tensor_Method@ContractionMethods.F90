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
! summary: Double contraction of rank-2 and Voigt rank-2 tensors
!
! For a Voigt tensor the tensor shear component is `Scale * V(k)`,
! with `k = 4, 5, 6` for the `12, 23, 13` pairs.

SUBMODULE(Rank2Tensor_Method) ContractionMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Contraction1
ans = SUM(obj1%T * obj2%T)
END PROCEDURE Rank2Tensor_Contraction1

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Contraction2
ASSOCIATE (T => obj1%T, V => obj2%V, s => obj2%scale)
  ans = T(1, 1) * V(1) + T(2, 2) * V(2) + T(3, 3) * V(3) &
        + s * ((T(1, 2) + T(2, 1)) * V(4) &
               + (T(2, 3) + T(3, 2)) * V(5) &
               + (T(1, 3) + T(3, 1)) * V(6))
END ASSOCIATE
END PROCEDURE Rank2Tensor_Contraction2

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Contraction3
ans = Rank2Tensor_Contraction2(obj1=obj2, obj2=obj1)
END PROCEDURE Rank2Tensor_Contraction3

!----------------------------------------------------------------------------
!                                                                Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Contraction4
ASSOCIATE (A => obj1%V, B => obj2%V)
  ans = A(1) * B(1) + A(2) * B(2) + A(3) * B(3) &
        + 2.0_DFP * obj1%scale * obj2%scale &
        * (A(4) * B(4) + A(5) * B(5) + A(6) * B(6))
END ASSOCIATE
END PROCEDURE Rank2Tensor_Contraction4

END SUBMODULE ContractionMethods
