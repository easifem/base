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

SUBMODULE( Rank2Tensor_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_by_mat
  Obj%T = Mat
  IF( PRESENT( isSym ) ) THEN
    Obj%isSym = isSym
  ELSE
    Obj%isSym = .FALSE.
  END IF
END PROCEDURE init_by_mat

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_by_voigt
  Obj%T = V
  Obj%isSym = .TRUE.
END PROCEDURE init_by_voigt

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_voigt_from_r2tensor
  CALL Initiate( Obj, T=T%T, VoigtType=VoigtType)
END PROCEDURE init_voigt_from_r2tensor

!----------------------------------------------------------------------------
!                                                                Rank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE r2t_by_mat
  CALL Initiate( Obj=Ans, Mat=Mat, isSym=isSym )
END PROCEDURE r2t_by_mat

!----------------------------------------------------------------------------
!                                                                Rank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE r2t_by_voigt
  CALL Initiate( Obj=Ans, V=V )
END PROCEDURE r2t_by_voigt

!----------------------------------------------------------------------------
!                                                        Rank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ptr_r2t_by_mat
  ALLOCATE( Ans )
  CALL Initiate( Obj=Ans, Mat=Mat, isSym=isSym )
END PROCEDURE ptr_r2t_by_mat

!----------------------------------------------------------------------------
!                                                        Rank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ptr_r2t_by_voigt
  ALLOCATE( Ans )
  CALL Initiate( Obj=Ans, V=V )
END PROCEDURE ptr_r2t_by_voigt

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE r2tensor_eq_mat
  CALL Initiate( Obj=Obj, Mat=Mat )
END PROCEDURE r2tensor_eq_mat

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_eq_r2tensor
  Mat = Obj%T
END PROCEDURE mat_eq_r2tensor

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_eq_r2tensor
  CALL Initiate( Obj=V, T=Obj%T, VoigtType=StressTypeVoigt)
END PROCEDURE voigt_eq_r2tensor

!----------------------------------------------------------------------------
!                                                                  Identity
!----------------------------------------------------------------------------

MODULE PROCEDURE identity_rank2
  CALL Initiate( Obj=Obj, Mat=Eye3, isSym=.TRUE. )
END PROCEDURE identity_rank2

!----------------------------------------------------------------------------
!                                                                       ones
!----------------------------------------------------------------------------

MODULE PROCEDURE ones_rank2
  REAL( DFP ) :: T( 3, 3 )
  T = 1.0_DFP
  CALL Initiate( Obj=Obj, Mat=T, isSym=.TRUE. )
END PROCEDURE ones_rank2

!----------------------------------------------------------------------------
!                                                                       zeros
!----------------------------------------------------------------------------

MODULE PROCEDURE zeros_rank2
  REAL( DFP ) :: T( 3, 3 )
  T = 0.0_DFP
  CALL Initiate( Obj=Obj, Mat=T, isSym=.TRUE. )
END PROCEDURE zeros_rank2

!----------------------------------------------------------------------------
!                                                             IsotropicTensor
!----------------------------------------------------------------------------

MODULE PROCEDURE isotropic_rank2
  CALL Initiate( Obj=Obj, Mat=Lambda*Eye3, isSym=.TRUE. )
END PROCEDURE isotropic_rank2

END SUBMODULE Constructor
