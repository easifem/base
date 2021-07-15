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

SUBMODULE( Rank2Tensor_Method ) InvarMethods
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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE trace_obj
  ASSOCIATE( T => obj%T )
  IF( PRESENT( Power ) ) THEN
    SELECT CASE( Power )
    CASE( 1 )
      Ans = T_11 + T_22 + T_33
    CASE( 2 )
      Ans = SUM( T * TRANSPOSE( T ) )
    CASE( 3 )
      Ans = SUM( MATMUL( T, T ) * TRANSPOSE( T ) )
    END SELECT
  ELSE
    Ans = T_11 + T_22 + T_33
  END IF
  END ASSOCIATE
END PROCEDURE trace_obj

!----------------------------------------------------------------------------
!                                                                       J2
!----------------------------------------------------------------------------

MODULE PROCEDURE j2_obj
  LOGICAL( LGT ) :: isDev
  isDev = INPUT( default = .FALSE., option = isDeviatoric )
  IF( isDev ) THEN
    Ans = 0.5_DFP * Trace(obj=obj, Power=2)
  ELSE
    ASSOCIATE( T => obj%T )
      Ans = ( T_11 - T_22 ) ** 2 &
        & + ( T_22 - T_33 ) ** 2 &
        & + ( T_33 - T_11 ) ** 2 &
        & + 6.0_DFP * ( T_12 * T_21 + T_23 * T_32 + T_13 * T_31 )
      Ans = Ans / 6.0_DFP
    END ASSOCIATE
  END IF
END PROCEDURE j2_obj

!----------------------------------------------------------------------------
!                                                                         J3
!----------------------------------------------------------------------------

MODULE PROCEDURE j3_obj
  LOGICAL( LGT ) :: isDev
  isDev = INPUT( default = .FALSE., option = isDeviatoric )
  IF( isDev ) THEN
    Ans = det( obj )
  ELSE
    Ans = det( Deviatoric( obj ) )
  END IF
END PROCEDURE j3_obj

!----------------------------------------------------------------------------
!                                                                         Det
!----------------------------------------------------------------------------

MODULE PROCEDURE det_obj
  ASSOCIATE( T => obj%T )
    Ans = T(1,1)*(T(2,2)*T(3,3)-T(2,3)*T(3,2)) &
          & - T(1,2)*(T(2,1)*T(3,3)-T(2,3)*T(3,1)) &
          & + T(1,3)*(T(2,1)*T(3,2)-T(3,1)*T(2,2))
  END ASSOCIATE
END PROCEDURE det_obj

!----------------------------------------------------------------------------
!                                                                   LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE theta_obj_j2j3
  REAL( DFP ) :: J_2, J_3, Dummy
  J_2 = J2; J_3 = J3
  IF( J_2 .EQ. 0.0_DFP ) THEN
    Ans = 0.0_DFP
  ELSE
    Dummy = 1.5_DFP * SQRT( 3.0_DFP ) * J_3 / ( J_2 * SQRT( J_2 ) )
    IF( Dummy .GE. 1.0_DFP ) Dummy = 1.0_DFP
    IF( Dummy .LE. -1.0_DFP ) Dummy = -1.0_DFP
    IF( LodeType .EQ. SineLode ) Ans = ASIN( -Dummy ) / 3.0_DFP
    IF( LodeType .EQ. CosineLode ) Ans = ACOS( Dummy ) / 3.0_DFP
  END IF
END PROCEDURE theta_obj_j2j3

!----------------------------------------------------------------------------
!                                                                   LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE theta_obj
  Ans = LodeAngle(LodeType=LodeType, &
    & J2=J2( obj, isDeviatoric ), &
    & J3=J3( obj, isDeviatoric ) )
END PROCEDURE theta_obj

!----------------------------------------------------------------------------
!                                                                   Isotropic
!----------------------------------------------------------------------------

MODULE PROCEDURE iso_part_obj
  CALL IsotropicTensor( obj=Ans, Lambda=Trace( obj ) / 3.0_DFP )
END PROCEDURE iso_part_obj

!----------------------------------------------------------------------------
!                                                                 Deviatoric
!----------------------------------------------------------------------------

MODULE PROCEDURE dev_part_obj
  REAL( DFP ) :: a
  ASSOCIATE( T => Ans%T )
  a = Trace( obj ) / 3.0_DFP
  T = 0.0_DFP
  T_11 = a
  T_22 = a
  T_33 = a
  T = -T + obj%T
  END ASSOCIATE
END PROCEDURE dev_part_obj

!----------------------------------------------------------------------------
!                                                                 Invariants
!----------------------------------------------------------------------------

MODULE PROCEDURE invariants_rank2
  LOGICAL( LGT ) :: isDev

  isDev = INPUT( default = .FALSE., option=isDeviatoric )
  IF( isDev ) THEN
    Ans( 1 ) = 0.0
    Ans( 2 ) = 0.5_DFP * Contraction( obj, TRANSPOSE( obj ) )
    Ans( 3 ) = Det( obj )
  ELSE
    Ans( 1 ) = Trace( obj )
    Ans( 2 ) = 0.5_DFP * ( Ans( 1 ) ** 2 -  Trace( obj, Power=2 ) )
    Ans( 3 ) = Det( obj )
  END IF
END PROCEDURE invariants_rank2

!----------------------------------------------------------------------------
!                                                                    Spectral
!----------------------------------------------------------------------------

MODULE PROCEDURE eigen_r2t
  REAL( DFP ) :: Mat( 3, 3 )
  Mat = obj%T
  IF( obj%isSym ) THEN
    CALL JacobiMethod( Mat=Mat, EigenValues=WR, EigenVectors=QR, MaxIter = 20)
  ELSE
    CALL spectral_r2t( obj%T, QR=QR, WR=WR, QI=QI, WI=WI )
  END IF
END PROCEDURE eigen_r2t

!----------------------------------------------------------------------------
!                                                                    Spectral
!----------------------------------------------------------------------------

SUBROUTINE spectral_r2t( T, QR, WR, QI, WI )
  REAL( DFP ), INTENT( IN ) :: T( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: QR( 3, 3 ), QI( 3, 3 )
  REAL( DFP ), INTENT( OUT ) :: WR( 3 ), WI( 3 )

  ! Define internal varuables
  REAL( DFP ) :: EigenVec( 3, 3 )
  REAL( DFP ) :: Mat( 3, 3 )

  Mat = T
  CALL GEEV( A = Mat, WR = WR, WI = WI, VR = EigenVec )

  ! First two eigen value is complex
  IF( ABS( WI( 1 ) ) .GT. Zero ) THEN
    QR( :, 1 ) = EigenVec( :, 1 )
    QI( :, 1 ) = EigenVec( :, 2 )
    QR( :, 2 ) = EigenVec( :, 1 )
    QI( :, 2 ) = -EigenVec( :, 2 )
    QR( :, 3 ) = EigenVec( :, 3 )
    QI( :, 3 ) = 0.0_DFP
  ! Last two eigen value is complex
ELSE IF( ABS( WI( 2 ) ) .GT. Zero ) THEN
    QR( :, 1 ) = EigenVec( :, 1 )
    QI( :, 1 ) = 0.0_DFP
    QR( :, 2 ) = EigenVec( :, 2 )
    QI( :, 2 ) = EigenVec( :, 3 )
    QR( :, 3 ) = EigenVec( :, 2 )
    QI( :, 3 ) = -EigenVec( :, 3 )
  ! no complex eigen value
  ELSE
    QI = 0.0_DFP
    QR( :, 1 ) = EigenVec( :, 1 )
    QR( :, 2 ) = EigenVec( :, 2 )
    QR( :, 3 ) = EigenVec( :, 3 )
  END IF
END SUBROUTINE spectral_r2t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pd_r2t
  CALL PD( Mat=obj%T, R=R%T, U=U%T, V=V%T )
  U%isSym = .TRUE.
  V%isSym = .TRUE.
END PROCEDURE pd_r2t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Polar decomposition
!
!### Introduction
! 	This subroutine calculates the polar decomposition
! * Ref: Higham and Noferini, 2015 Algorithm 3.1 for NSD = 3
! * PDType = 1 for F = RU; and 2 for F = VR
! * Mat = RU = VR, Therefore H denotes either U or V

SUBROUTINE PD( Mat, R, U, V )
  ! Define intent of dummy variables
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: R( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: U( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: V( 3, 3 )

  ! Define internal variables
  REAL( DFP ) :: RT( 3, 3 )
  REAL( DFP ) :: B( 4, 4 )
  REAL( DFP ) :: EigenVecs( 4, 4 )
  REAL( DFP ) :: EigenVals( 4 )
  REAL( DFP ) :: Vmax( 4 )
  INTEGER( I4B ) :: MAX_LOC( 1 )

  B( 1, 1 ) = Mat( 1, 1 ) + Mat( 2, 2 ) + Mat( 3, 3 )
  B( 1, 2 ) = Mat( 2, 3 ) - Mat( 3, 2 )
  B( 1, 3 ) = Mat( 3, 1 ) - Mat( 1, 3 )
  B( 1, 4 ) = Mat( 1, 2 ) - Mat( 2, 1 )
  B( 2, 1 ) = Mat( 1, 2 )
  B( 2, 2 ) = Mat( 1, 1 ) - Mat( 2, 2 ) -  Mat( 3, 3 )
  B( 2, 3 ) = Mat( 1, 2 ) + Mat( 2, 1 )
  B( 2, 4 ) = Mat( 1, 3 ) + Mat( 3, 1 )
  B( 3, 1 ) = B( 1, 3 )
  B( 3, 2 ) = B( 2, 3 )
  B( 3, 3 ) = - Mat( 1, 1 ) + Mat( 2, 2 ) - Mat( 3, 3 )
  B( 3, 4 ) = Mat( 2, 3 ) + Mat( 3, 2 )
  B( 4, 1 ) = B( 1, 4 )
  B( 4, 2 ) = B( 2, 4 )
  B( 4, 3 ) = B( 3, 4 )
  B( 4, 4 ) = - Mat( 1, 1 ) - Mat( 2, 2 ) + Mat( 3, 3 )

  CALL JacobiMethod( &
    & Mat = B, EigenValues  = EigenVals,  &
    & EigenVectors = EigenVecs, MaxIter = 20 )

  ! Get Dominating eigen value and corresponding eigen vectors
  MAX_LOC = MAXLOC ( ABS( EigenVals ) )
  Vmax = EigenVecs( :, MAX_LOC( 1 ) )

  ! Compute R matrix from Vmax Vector
  R( 1, 1 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 3 ) * Vmax( 3 ) + Vmax( 4 ) * Vmax( 4 ) )
  RT( 1, 1 ) = R( 1, 1 )

  R( 1, 2 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 3 ) + Vmax( 1 ) * Vmax( 4 ) )
  RT( 2, 1 ) = R( 1, 2 )

  R( 1, 3 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 4 ) - Vmax( 1 ) * Vmax( 3 ) )
  RT( 3, 1 ) = R( 1, 3 )

  R( 2, 1 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 3 ) - Vmax( 1 ) * Vmax( 4 ) )
  RT( 1, 2 ) = R( 2, 1 )

  R( 2, 2 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 2 ) * Vmax( 2 ) + Vmax( 4 ) * Vmax( 4 ) )
  RT( 2, 2 ) = R( 2, 2 )

  R( 2, 3 ) = 2.0_DFP * ( Vmax( 3 ) * Vmax( 4 ) + Vmax( 1 ) * Vmax( 2 ) )
  RT( 3, 2 ) = R( 2, 3 )

  R( 3, 1 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 4 ) + Vmax( 1 ) * Vmax( 3 ) )
  RT( 1, 3 ) = R( 3, 1 )

  R( 3, 2 ) = 2.0_DFP * ( Vmax( 3 ) * Vmax( 4 ) - Vmax( 1 ) * Vmax( 2 ) )
  RT( 2, 3 ) = R( 3, 2 )

  R( 3, 3 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 3 ) * Vmax( 3 ) + Vmax( 2 ) * Vmax( 2 ) )
  RT( 3, 3 ) = R( 3, 3 )

  U = MATMUL( RT, Mat )
  V = MATMUL( Mat, RT )
END SUBROUTINE PD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#undef T_11
#undef T_12
#undef T_13
#undef T_21
#undef T_22
#undef T_23
#undef T_31
#undef T_32
#undef T_33

END SUBMODULE InvarMethods
