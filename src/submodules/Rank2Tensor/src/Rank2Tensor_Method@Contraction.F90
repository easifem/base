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
! summary: 	Submodules for computing the contraction

SUBMODULE(Rank2Tensor_Method) Contraction
USE BaseMethod
#define T_11 T( 1, 1 )
#define T_12 T( 1, 2 )
#define T_13 T( 1, 3 )
#define T_21 T( 2, 1 )
#define T_22 T( 2, 2 )
#define T_23 T( 2, 3 )
#define T_31 T( 3, 1 )
#define T_32 T( 3, 2 )
#define T_33 T( 3, 3 )
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE r2_contract_r2
  Ans = SUM( obj1%T * obj2%T )
END PROCEDURE r2_contract_r2

!----------------------------------------------------------------------------
!                                                              Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE r2_contract_voigt_r2
  ASSOCIATE( T => obj1%T, V => obj2%V, Scale => obj2%Scale )
  Ans = T_11 * V( 1 ) &
    & + T_22 * V( 2 ) &
    & + T_33 * V( 3 ) &
    & + (T_12 + T_21) * V( 4 ) * Scale &
    & + (T_23 + T_32) * V( 5 ) * Scale &
    & + (T_13 + T_31) * V( 6 ) * Scale
  END ASSOCIATE
END PROCEDURE r2_contract_voigt_r2

!----------------------------------------------------------------------------
!                                                              Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_r2_contract_r2
  Ans = r2_contract_voigt_r2( obj1=obj2, obj2=obj1 )
END PROCEDURE voigt_r2_contract_r2

!----------------------------------------------------------------------------
!                                                              Contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_r2_contract_voigt_r2
  ASSOCIATE( A => obj1%V, B => obj2%V, S1 => obj1%Scale, S2 => obj2%Scale )
    Ans = A( 1 ) * B( 1 ) + A( 2 ) * B( 2 ) + A( 3 ) * B( 3 ) &
      & + 2.0 * S1 * S2 * ( A( 4 ) * B( 4 ) &
      & + A( 5 ) * B( 5 ) + A( 6 ) * B( 6 ) )
  END ASSOCIATE
END PROCEDURE voigt_r2_contract_voigt_r2

#undef T_11
#undef T_12
#undef T_13
#undef T_21
#undef T_22
#undef T_23
#undef T_31
#undef T_32
#undef T_33
END SUBMODULE Contraction