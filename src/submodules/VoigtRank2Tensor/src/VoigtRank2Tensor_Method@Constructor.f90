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

SUBMODULE( VoigtRank2Tensor_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_from_vec
ASSOCIATE( V => Obj%V, Scale => Obj%Scale )
  V = 0.0_DFP
  Obj%VoigtType = VoigtType
  V( 1:6 ) = Vec( 1:6 )
  SELECT CASE( VoigtType )
  CASE( StrainTypeVoigt )
    Scale = 0.5_DFP
  CASE( StressTypeVoigt )
    Scale = 1.0_DFP
  END SELECT
END ASSOCIATE
END PROCEDURE init_from_vec

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_from_mat
ASSOCIATE( V => Obj%V, Scale => Obj%Scale )
  Obj%VoigtType = VoigtType
  V( 1 ) = T( 1, 1 )
  V( 2 ) = T( 2, 2 )
  V( 3 ) = T( 3, 3 )
  V( 4 ) = T( 1, 2 ) + T( 2, 1 )
  V( 5 ) = T( 2, 3 ) + T( 3, 2 )
  V( 6 ) = T( 1, 3 ) + T( 3, 1 )
  SELECT CASE( VoigtType )
  CASE( StressTypeVoigt )
    Scale = 1.0_DFP
    V( 4 : 6 ) = 0.5_DFP * V( 4 : 6 )
  CASE( StrainTypeVoigt )
    Scale = 0.5_DFP
  END SELECT
END ASSOCIATE
END PROCEDURE init_from_mat

!----------------------------------------------------------------------------
!                                                            VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor1
  CALL Initiate( Ans, Vec, VoigtType )
END PROCEDURE constructor1

!----------------------------------------------------------------------------
!                                                            VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor2
  CALL Initiate( Obj=Ans, T=T, VoigtType=VoigtType )
END PROCEDURE constructor2

!----------------------------------------------------------------------------
!                                                   VoigtRank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor_1
  ALLOCATE( Ans )
  CALL Initiate( Obj=Ans, Vec=Vec, VoigtType = VoigtType )
END PROCEDURE constructor_1

!----------------------------------------------------------------------------
!                                                  VoigtRank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor_2
  ALLOCATE( Ans )
  CALL Initiate( Obj=Ans, T=T, VoigtType=VoigtType )
END PROCEDURE constructor_2

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_eq_obj
  ASSOCIATE( V => Obj%V, Scale => Obj%Scale )
  T = 0.0_DFP
  T( 1, 1 ) = V( 1 )
  T( 2, 2 ) = V( 2 )
  T( 3, 3 ) = V( 3 )
  T( 1, 2 ) = Scale * V( 4 )
  T( 2, 1 ) = T( 1, 2 )
  T( 2, 3 ) = Scale * V( 5 )
  T( 3, 2 ) = T( 2, 3 )
  T( 1, 3 ) = Scale * V( 6 )
  T( 3, 1 ) = T( 1, 3 )
  END ASSOCIATE
END PROCEDURE mat_eq_obj

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_eq_obj
  vec = Obj%V
END PROCEDURE vec_eq_obj

END SUBMODULE Constructor
